(in-package #:autolith)

;;;; -- World Store Tests --

(-> world-tests--workspace (pathname string) string)
(defun world-tests--workspace (root name)
  "Create and return the canonical name of workspace NAME below ROOT."
  (let ((directory (merge-pathnames (format nil "~A/" name) root)))
    (ensure-directories-exist (merge-pathnames "marker" directory))
    (workspace-directory-name directory)))

(-> test-world-persistence () null)
(defun test-world-persistence ()
  "Test participants and channels persist with derived residents and subscribers."
  (with-test-configuration (configuration root)
    (let ((workspace (world-tests--workspace root "project")))
      (multiple-value-bind (human home)
          (world-participant-create configuration
                                    :kind  ':human
                                    :name  "user"
                                    :home  (list :workspace workspace :title "Lobby")
                                    :now   1000)
        (test-assert (and (eq (participant-kind human) ':human)
                          (string= (participant-residence human) (channel-identifier home))
                          (string= (channel-owner home) (participant-identifier human))
                          (string= (channel-workspace home) workspace)
                          (string= (channel-title home) "Lobby"))
                     "a participant created with a home owns and resides in that channel")
        (let* ((side  (world-channel-create configuration
                                            :id        "b-side"
                                            :owner     (participant-identifier human)
                                            :workspace workspace
                                            :parent    (channel-identifier home)
                                            :now       1001))
               (agent (world-participant-create configuration
                                                :kind          ':agent
                                                :name          "autolith"
                                                :residence     (channel-identifier side)
                                                :parent        (participant-identifier human)
                                                :settings      '(:model "gpt-5" :reasoning-effort "medium")
                                                :now           1002)))
          (test-assert (and (string= (channel-identifier side) "b-side")
                            (equal (channel-parent side) (channel-identifier home)))
                       "an explicit channel identifier and parent are kept")
          (world-participant-subscribe configuration (participant-identifier human) "b-side"
                                       :now 1003)
          (world-participant-move configuration (participant-identifier agent)
                                  (channel-identifier home) :now 1004)
          (world-participant-update configuration (participant-identifier agent)
                                    :presence ':present :now 1005)
          (world-channel-update configuration "b-side" :title "Side work" :now 1006)
          (let* ((world    (world-load configuration))
                 (agent    (world-participant-find world (participant-identifier agent)))
                 (human    (world-participant-find world (participant-identifier human)))
                 (side     (world-channel-find world "b-side")))
            (test-assert (and (string= (participant-residence agent) (channel-identifier home))
                              (eq (participant-presence agent) ':present)
                              (= (participant-updated-at agent) 1005)
                              (equal (participant-settings agent)
                                     '(:model "gpt-5" :reasoning-effort "medium")))
                         "participant moves and updates reload from the log")
            (test-assert (and (string= (channel-title side) "Side work")
                              (= (channel-updated-at side) 1006))
                         "channel updates reload from the log")
            (test-assert (equal (mapcar #'participant-identifier
                                        (world-channel-residents world (channel-identifier home)))
                                (list (participant-identifier agent)
                                      (participant-identifier human)))
                         "residents are derived from residence, newest change first")
            (test-assert (null (world-channel-residents world "b-side"))
                         "a channel left by its only resident is empty")
            (test-assert (equal (mapcar #'participant-identifier
                                        (world-channel-subscribers world "b-side"))
                                (list (participant-identifier human)))
                         "subscribers are derived from subscriptions")
            (test-assert (equal (mapcar #'channel-identifier (world-channels world))
                                (list "b-side" (channel-identifier home)))
                         "channels list most recently changed first")
            (test-assert (equal (mapcar #'channel-identifier
                                        (world-channel-children world (channel-identifier home)))
                                (list "b-side"))
                         "nested channels are found by parent")
            (test-assert (eq (world-human-participant world) human)
                         "the human participant is found by kind")
            (test-assert (equal (participant-subscriptions human) '("b-side"))
                         "subscriptions are stored on the participant")
            (world-participant-unsubscribe configuration (participant-identifier human) "b-side"
                                           :now 1007)
            (test-assert (null (participant-subscriptions
                                (world-participant-find (world-load configuration)
                                                        (participant-identifier human))))
                         "unsubscribing removes the channel")
            (test-assert (handler-case
                             (progn (world-participant-move configuration
                                                            (participant-identifier agent)
                                                            "missing")
                                    nil)
                           (world-error (condition)
                             (eq (world-error-operation condition) ':move-participant)))
                         "moving into an unknown channel is refused")
            (test-assert (handler-case
                             (progn (world-participant-create configuration
                                                              :kind      ':agent
                                                              :name      "two homes"
                                                              :residence "b-side"
                                                              :home      (list :workspace workspace))
                                    nil)
                           (world-error () t))
                         "a participant takes either a residence or a home, not both"))))))
  nil)

(-> test-world-deletion-rules () null)
(defun test-world-deletion-rules ()
  "Test deletion keeps residents explicit and channels under human control."
  (with-test-configuration (configuration root)
    (let ((workspace (world-tests--workspace root "project")))
      (multiple-value-bind (human lobby)
          (world-participant-create configuration
                                    :kind ':human
                                    :name "user"
                                    :home (list :workspace workspace :id "b-lobby")
                                    :now  2000)
        (multiple-value-bind (lead task)
            (world-participant-create configuration
                                      :kind   ':agent
                                      :name   "lead"
                                      :parent (participant-identifier human)
                                      :home   (list :workspace workspace :id "b-task"
                                                    :parent (channel-identifier lobby))
                                      :now    2001)
          (let ((helper (world-participant-create configuration
                                                  :kind      ':agent
                                                  :name      "helper"
                                                  :parent    (participant-identifier lead)
                                                  :residence (channel-identifier task)
                                                  :now       2002)))
            (world-participant-subscribe configuration (participant-identifier human)
                                         (channel-identifier task))
            (test-assert (handler-case
                             (progn (world-channel-delete configuration "b-task") nil)
                           (world-error (condition)
                             (eq (world-error-operation condition) ':delete-channel)))
                         "a channel with residents is not deleted implicitly")
            (test-assert (handler-case
                             (progn (world-channel-delete configuration "b-lobby") nil)
                           (world-error () t))
                         "a channel with nested channels is refused")
            (test-assert (handler-case
                             (progn (world-participant-delete configuration
                                                              (participant-identifier lead))
                                    nil)
                           (world-error () t))
                         "a participant with children needs a recursive deletion")
            (test-assert (handler-case
                             (progn (world-participant-delete configuration
                                                              (participant-identifier human)
                                                              :recursive-p t)
                                    nil)
                           (world-error () t))
                         "deleting the only human would leave channels without a human owner")
            (let ((deleted (world-channel-delete configuration "b-task"
                                                 :participants (list (participant-identifier lead)
                                                                     (participant-identifier helper))
                                                 :now          2003)))
              (test-assert (equal deleted (list (participant-identifier lead)
                                                (participant-identifier helper)))
                           "deleting a channel with its listed residents reports every deleted participant")
              (let* ((world (world-load configuration))
                     (human (world-participant-find world (participant-identifier human))))
                (test-assert (and (null (world-channel-find world "b-task"))
                                  (null (world-participant-find world (participant-identifier lead)))
                                  (null (world-participant-find world (participant-identifier helper)))
                                  (world-channel-find world "b-lobby"))
                             "the channel and its residents are gone, the parent stays")
                (test-assert (null (participant-subscriptions human))
                             "subscriptions to a deleted channel are dropped")))
            (let ((agent (world-participant-create configuration
                                                  :kind      ':agent
                                                  :name      "owner"
                                                  :parent    (participant-identifier human)
                                                  :residence "b-lobby"
                                                  :now       2004)))
              (world-channel-create configuration
                                    :id        "b-owned"
                                    :owner     (participant-identifier agent)
                                    :workspace workspace
                                    :now       2005)
              (world-participant-delete configuration (participant-identifier agent) :now 2006)
              (test-assert (string= (channel-owner (world-channel-find (world-load configuration)
                                                                       "b-owned"))
                                    (participant-identifier human))
                           "channels of a deleted participant pass to its parent")))))))
  nil)

(-> test-world-malformed-state () null)
(defun test-world-malformed-state ()
  "Test malformed world data is reported without reader evaluation or partial state."
  (with-test-configuration (configuration)
    (let ((pathname                   (configuration-world-path configuration))
          (*world-reader-evaluated-p* nil))
      (declare (special *world-reader-evaluated-p*))
      (ensure-directories-exist pathname)
      (with-open-file (stream pathname
                              :direction         ':output
                              :if-exists         ':supersede
                              :if-does-not-exist ':create)
        (write-string "(:world :version 1)" stream)
        (terpri stream)
        (write-string "#.(setf autolith::*world-reader-evaluated-p* t)" stream))
      (test-assert (handler-case (progn (world-load configuration) nil)
                     (world-error () t))
                   "malformed world data signals a world error")
      (test-assert (null *world-reader-evaluated-p*)
                   "world data disables reader evaluation")
      (with-open-file (stream pathname
                              :direction         ':output
                              :if-exists         ':supersede
                              :if-does-not-exist ':create)
        (write-string "(:world :version 1)" stream)
        (terpri stream)
        (write-string "(:participant :version 1 :id \"p\" :kind :agent :name \"x\" :residence \"missing\" :subscriptions nil :presence :offline :settings nil :created-at 1 :updated-at 1)"
                      stream))
      (test-assert (handler-case (progn (world-load configuration) nil)
                     (world-error (condition)
                       (equal (world-error-identifier condition) "p")))
                   "a dangling residence is reported with its participant")))
  nil)
