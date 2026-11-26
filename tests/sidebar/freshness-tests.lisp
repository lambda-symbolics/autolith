(in-package #:autolith)

(-> test-world-sidebar-freshness () null)
(defun test-world-sidebar-freshness ()
  "Retain durable rows and selection through failed gathers, then recover runtime state."
  (with-test-configuration (configuration)
    (world-tests--persisted-conversation configuration "bfirst1" 1000)
    (world-server--reconcile configuration)
    (let* ((clock   0)
           (fail-p  nil)
           (sidebar (world-sidebar-create configuration :clock-function (lambda () clock))))
      (setf (world-sidebar-viewport-height sidebar) 10)
      (test-call-with-function-replacements
       (list (list 'world-sidebar--statuses
                   (lambda (candidate)
                     (declare (ignore candidate))
                     (when fail-p (error "private-response-payload"))
                     (values (list (list :session-id "bfirst1" :state ':idle
                                         :terminal-attached-p t :message-count 5)) t))))
       (lambda ()
         ;; Simulate a long-lived panel whose formerly hosted channels were removed.
         (loop for n below 256
               for id = (format nil "retired-~D" n)
               do (setf (gethash id (world-sidebar-seen sidebar)) 8
                        (gethash id (world-sidebar-mentions-seen sidebar)) 2
                        (gethash id (world-sidebar-toggled sidebar)) ':closed
                        (gethash (cons nil id) (world-sidebar-pathnames sidebar)) #P"old.sexp"))
         (screen-panel-refresh sidebar)
         (test-assert (and (<= (hash-table-count (world-sidebar-pathnames sidebar)) 2)
                           (<= (hash-table-count (world-sidebar-seen sidebar)) 2)
                           (<= (hash-table-count (world-sidebar-mentions-seen sidebar)) 2)
                           (zerop (hash-table-count (world-sidebar-toggled sidebar))))
                      "a fresh catalogue releases retired channels' locator, counter and fold entries")
         (let ((position (position "bfirst1" (world-sidebar-tests--keys sidebar) :test #'equal)))
           (test-assert position "the durable channel is present")
           (setf (world-sidebar-selected sidebar) position
                 fail-p t)
           (incf clock (* 2 internal-time-units-per-second))
           (test-assert (screen-panel-refresh sidebar) "a failed gather changes runtime availability")
           (let ((row (nth position (world-sidebar-entries sidebar))))
             (test-assert (and (equal (sidebar-entry-identifier row) "bfirst1")
                               (eq (sidebar-channel-state row) ':unknown)
                               (not (sidebar-channel-attached-p row))
                               (= position (world-sidebar-selected sidebar))
                               (= 0 (world-sidebar-last-success-at sidebar))
                               (world-sidebar-failure sidebar)
                               (not (search "payload" (world-sidebar-failure sidebar))))
                          "stale rows retain identity without claiming live attachment or exposing response data"))
           (setf fail-p nil)
           (incf clock (* 2 internal-time-units-per-second))
           (test-assert (screen-panel-refresh sidebar) "a later successful gather restores runtime state")
           (let ((row (nth position (world-sidebar-entries sidebar))))
             (test-assert (and (eq (sidebar-channel-state row) ':idle)
                               (sidebar-channel-attached-p row)
                               (= clock (world-sidebar-last-success-at sidebar))
                               (null (world-sidebar-failure sidebar)))
                          "successful observations clear failure provenance")))))))
  (with-test-configuration (configuration root)
    (let* ((workspace (merge-pathnames "reused-workspace/" root))
           (admin-a   (merge-pathnames "repository-a/" root))
           (admin-b   (merge-pathnames "repository-b/" root))
           (sidebar   (world-sidebar-create configuration :clock-function (constantly 0))))
      (dolist (admin (list admin-a admin-b))
        (ensure-directories-exist admin))
      (world-repository-tests--git root (list "init" "-q" "--separate-git-dir" (namestring admin-a)
                                             (namestring workspace)))
      (world-repository-tests--git workspace '("commit" "--allow-empty" "-qm" "repository-a"))
      (multiple-value-bind (human home)
          (world-participant-create configuration :kind ':human :name "repository-owner"
                                    :home (list :workspace (namestring workspace)))
        (declare (ignore human))
        (test-call-with-function-replacements
         (list (list 'world-sidebar--statuses
                     (lambda (candidate) (declare (ignore candidate)) (values nil t))))
         (lambda ()
           (world-sidebar--gather sidebar)
           (let ((memo (world-sidebar-repository-memo sidebar)))
             (world-sidebar--gather sidebar)
             (test-assert (eq memo (world-sidebar-repository-memo sidebar))
                          "Unchanged repository identities reuse their memo inside its lifetime."))
           (platform-delete-directory-tree *platform* workspace :validate t)
           (world-repository-tests--git root (list "init" "-q" "--separate-git-dir" (namestring admin-b)
                                                  (namestring workspace)))
           (world-repository-tests--git workspace '("commit" "--allow-empty" "-qm" "repository-b"))
           (let* ((updated (world-channel-update configuration (channel-identifier home)
                                                 :workspace (namestring workspace)))
                  (key     (channel-repository-key updated)))
             (test-assert (and key (not (equal key (channel-repository-key home))))
                          "An explicit fresh observation captures the replacement repository at the same workspace.")
             (let ((gathered (world-sidebar--gather sidebar)))
               (test-assert (and (equal key (world-repository-group-key (first (getf gathered :groups))))
                                 (equal key (channel-repository-key
                                             (world-channel-find (world-load configuration)
                                                                 (channel-identifier home)))))
                            "A memo inside its lifetime cannot overwrite a newer saved repository identity."))
             (platform-delete-directory-tree *platform* workspace :validate t)
             (let* ((loaded (world-channel-find (world-load configuration) (channel-identifier home)))
                    (group  (first (world-repository-groups (list loaded) nil))))
               (test-assert (and (equal key (channel-repository-key loaded))
                                 (equal key (world-repository-group-key group))
                                 (not (world-repository-group-origin-observed-p group)))
                            "The replacement repository identity survives removal and durable reload."))))))))
  nil)

(-> test-world-sidebar-first-failure () null)
(defun test-world-sidebar-first-failure ()
  "Expose unavailable runtime observations even when the initial gather fails."
  (with-test-configuration (configuration)
    (let ((sidebar (world-sidebar-create configuration)))
      (test-call-with-function-replacements
       (list (list 'world-sidebar--gather (lambda (candidate)
                                           (declare (ignore candidate))
                                           (error "unavailable"))))
       (lambda ()
         (test-assert (and (screen-panel-refresh sidebar)
                           (getf (world-sidebar-inputs sidebar) :runtime-unknown-p)
                           (null (world-sidebar-last-success-at sidebar)))
                      "the initial failure is visible before any successful runtime observation")))))
  nil)

(-> test-world-sidebar-change-feed () null)
(defun test-world-sidebar-change-feed ()
  "Follow durable changes, cross-process invalidations, inbox transitions and reconnection in two clients."
  (world-server-tests--call
   (lambda (server configuration root)
     (declare (ignore server))
     (let* ((home         (world-ensure-inbox configuration))
            (person       (world-human-participant (world-load configuration)))
            (channel      (world-channel-create configuration :owner (participant-identifier person)
                                                :workspace           (world-tests--workspace root "reactive-sidebar")
                                                :title               "Initial request"))
            (identifier   (channel-identifier channel))
            (sidebars     (list (world-sidebar-create configuration) (world-sidebar-create configuration)))
            (observe      (symbol-function 'world-server--channel-snapshots))
            (observations 0))
       (labels ((caught-up-p (title)
                  "Take pending observations until both clients show TITLE."
                  (every (lambda (sidebar)
                           (screen-panel-refresh sidebar)
                           (let ((row (find identifier (getf (world-sidebar-inputs sidebar) :channels)
                                            :test #'equal :key #'channel-identifier)))
                             (and row (equal (channel-title row) title))))
                         sidebars))

                (pending-p (count)
                  "Take pending observations until both clients show COUNT waiting deliveries."
                  (every (lambda (sidebar)
                           (screen-panel-refresh sidebar)
                           (= count (or (rest (assoc identifier (getf (world-sidebar-inputs sidebar) :pending)
                                                    :test #'equal)) 0)))
                         sidebars)))
         (test-call-with-function-replacements
          (list (list 'world-server--channel-snapshots
                      (lambda (candidate)
                        (incf observations)
                        (funcall observe candidate))))
          (lambda ()
            (unwind-protect
                 (progn
                   (mapc #'world-sidebar-start sidebars)
                   (test-assert (task-tests--wait-until (lambda () (caught-up-p "Initial request")) 5)
                                "two subscribed clients receive the initial durable catalogue")
                   (let ((before observations))
                     (world-channel-update configuration identifier :title "Changed request")
                     (test-assert (and (task-tests--wait-until (lambda () (caught-up-p "Changed request")) 5)
                                       (= observations (1+ before)))
                                  "a committed title update reaches both clients through one shared runtime observation"))
                   (let ((*world-change-listeners* nil))
                     (world-channel-update configuration identifier :title "Relayed request"))
                   (test-assert (task-tests--wait-until (lambda () (caught-up-p "Relayed request")) 5)
                                "a writer without the local listener reaches both clients through the world endpoint")
                   (world-inbox-append
                    configuration identifier
                    (list (make-instance 'world-delivery :identifier "reactive-message" :time (get-universal-time)
                                         :sender (participant-identifier person) :sender-name (participant-name person)
                                         :sender-kind ':human :source (channel-identifier home) :target identifier
                                         :text "A waiting message" :kind ':post)))
                   (test-assert (task-tests--wait-until (lambda () (pending-p 1)) 5)
                                "an inbox commit pushes pending delivery counts to both clients")
                   (world-inbox-settle configuration identifier (list "reactive-message"))
                   (test-assert (task-tests--wait-until (lambda () (pending-p 0)) 5)
                                "settling a delivery pushes the next inbox transition")
                   (let* ((sidebar  (first sidebars))
                          (previous (world-sidebar-connection sidebar)))
                     (world-terminal-client-close previous)
                     (test-assert
                      (task-tests--wait-until
                       (lambda ()
                         (and (world-sidebar-connection sidebar)
                              (not (eq previous (world-sidebar-connection sidebar)))
                              (not (world-sidebar-connection-closed-p sidebar)))) 5)
                      "a disconnected notification subscription is replaced")
                     (world-channel-update configuration identifier :title "After reconnection")
                     (test-assert (task-tests--wait-until (lambda () (caught-up-p "After reconnection")) 5)
                                  "reconnection restores later updates for both clients")))
              (mapc #'world-sidebar-stop sidebars))))))))
  nil)

(-> test-world-change-feed-shutdown () null)
(defun test-world-change-feed-shutdown ()
  "Refuse an in-flight subscription whose admission races with feed shutdown."
  (world-server-tests--call
   (lambda (server configuration root)
     (declare (ignore root))
     (let ((serve      (symbol-function 'world-change-feed-serve))
           (arrived    (sb-thread:make-semaphore))
           (resume     (sb-thread:make-semaphore))
           (connection nil)
           (failure    nil)
           (connector  nil))
       (test-call-with-function-replacements
        (list (list 'world-change-feed-serve
                    (lambda (feed &rest arguments)
                      (sb-thread:signal-semaphore arrived)
                      (sb-thread:wait-on-semaphore resume)
                      (apply serve feed arguments))))
        (lambda ()
          (unwind-protect
               (progn
                 (setf connector
                       (make-thread
                        (lambda ()
                          (handler-case
                              (setf connection
                                    (world-terminal-client-connect
                                     (world-client-entry configuration) :operation ':watch-world
                                     :event-function                               (constantly nil)))
                            (world-terminal-error (condition)
                              (setf failure condition))))
                        :name "Autolith pending world subscriber"))
                 (test-assert (sb-thread:wait-on-semaphore arrived :timeout 5)
                              "the subscription reaches the serving endpoint before shutdown")
                 (world-change-feed-close (world-server-change-feed server))
                 (sb-thread:signal-semaphore resume)
                 (sb-thread:join-thread connector)
                 (test-assert (and failure (null connection))
                              "shutdown refuses the already-arrived subscription instead of admitting an orphan"))
            (sb-thread:signal-semaphore resume)
            (when connector
              (sb-thread:join-thread connector :default nil))
            (when connection
              (world-terminal-client-close connection))))))))
  nil)

(-> test-world-sidebar-runtime-invalidations () null)
(defun test-world-sidebar-runtime-invalidations ()
  "Refresh cached runtime state for reader ownership, queues and job lifecycle, excluding progress ticks."
  (world-server-tests--call
   (lambda (server configuration root)
     (let* ((opened      (world-server-tests--open (world-client-entry configuration)
                                                  (world-tests--workspace root "runtime-invalidations")))
            (identifier  (getf (rest opened) :session-id))
            (application (world-host-application (world-server--find-host server identifier)))
            (controller  (application-input-controller application))
            (hub         (application-client-hub application))
            (notify      (client-hub-change-function hub))
            (changes     0))
       (labels ((status ()
                  "Read this channel through the world's shared snapshot cache."
                  (rest (find identifier (world-server-channels server) :test #'equal
                              :key                                            (lambda (record) (getf (rest record) :session-id)))))

                (publish-job (duration)
                  "Publish the same running job with its changing elapsed duration."
                  (application-clients-publish
                   application
                   (list :ui         :operation ':command-activities
                         :activities (list (list :id "reactive-job" :state ':running
                                                 :duration-ms duration))))))
         (test-assert (not (getf (status) :reader-paused-p)) "the ready channel accepts terminal input")
         (application-input-controller-call-with-reader-paused
          controller
          (lambda ()
            (test-assert (getf (status) :reader-paused-p)
                         "a reader ownership transition invalidates the cached runtime observation")))
         (test-assert (not (getf (status) :reader-paused-p))
                      "releasing terminal ownership refreshes the cached observation again")
         (let ((first-client nil)
               (next-client  nil)
               (endpoint     (application-channel-endpoint application)))
           (unwind-protect
                (progn
                  (setf first-client (test-world-terminal--present endpoint :client-pid 424241))
                  (test-assert (eql 424241 (getf (status) :terminal-client-pid))
                               "the first controller appears in the shared runtime observation")
                  (setf next-client (test-world-terminal--present endpoint :client-pid 424242))
                  (test-assert (eql 424242 (getf (status) :terminal-client-pid))
                               "a second attachment invalidates the previous controlling terminal")
                  (world-terminal-client-call first-client ':state)
                  (test-assert (eql 424241 (getf (status) :terminal-client-pid))
                               "input from an attached terminal refreshes controller ownership"))
             (when next-client (world-terminal-client-close next-client))
             (when first-client (world-terminal-client-close first-client)))
           (test-assert (test-world-terminal--await-served endpoint)
                        "both controller transports finish before later state transitions"))
         (setf (client-hub-change-function hub) (lambda () (incf changes) (funcall notify)))
         (unwind-protect
              (progn
                (terminal-ui-set-pending-inputs (application-ui application) nil '("queued request"))
                (test-assert (plusp changes) "a changed pending-input projection invalidates runtime state")
                (publish-job 0)
                (let ((before changes))
                  (publish-job 250)
                  (test-assert (= before changes) "elapsed time alone reuses the runtime observation")
                  (application-clients-publish application '(:ui :operation :command-activities :activities nil))
                  (test-assert (= (1+ before) changes) "finishing the job invalidates its previous live state")))
           (setf (client-hub-change-function hub) notify))))))
  nil)
