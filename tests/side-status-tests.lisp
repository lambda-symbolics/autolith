(in-package #:autolith)

;;;; -- Live Side Status Tests --

(-> side-status-tests--records (conversation) list)
(defun side-status-tests--records (conversation)
  "Return only dedicated display records from CONVERSATION's durable history."
  (let ((records nil))
    (conversation-replay--map-records
     conversation
     (lambda (record)
       (when (eq (first record) ':side-status)
         (push record records))))
    (nreverse records)))

(-> side-status-tests--evidence (configuration conversation) t)
(defun side-status-tests--evidence (configuration conversation)
  "Return the actual non-compaction user-operation contribution evidence."
  (let ((contribution
          (user-operation-context
           (make-instance 'request-context
                          :configuration   configuration
                          :conversation    conversation
                          :tool-namespaces #()))))
    (and contribution (context-contribution-evidence contribution))))

(-> test-side-status-snapshot () null)
(defun test-side-status-snapshot ()
  "Read live clocks, focus, presentations, queue, and step without mutating them."
  (multiple-value-bind (application root)
      (lisp-machine-tests--application)
    (unwind-protect
         (let ((ui (terminal-ui-create
                    :terminal       (make-instance 'recording-terminal :columns 100)
                    :clock-function (constantly 70))))
           (setf (application-ui application) ui
                 (application-goal application) '(:objective "Implement feature")
                 (terminal-ui-status ui) "checking changes"
                 (terminal-ui-status-started-at ui) 10
                 (terminal-ui-status-progress-at ui) 60
                 (terminal-ui-status-worked-seconds ui) 120
                 (terminal-ui-agent-activities ui)
                 '((:state :running :observed-at 10)
                   (:state :queued :observed-at 20))
                 (terminal-ui-steering-input-previews ui) '("steering")
                 (terminal-ui-queued-input-previews ui) '("one" "two"))
           (let* ((before   (list (terminal-ui-status-started-at ui)
                                  (terminal-ui-status-progress-at ui)
                                  (conversation-next-sequence
                                   (application-conversation application))))
                  (snapshot (application-side-status-snapshot application)))
             (test-assert
              (and (equal (getf snapshot :focus) "Implement feature")
                   (= (getf snapshot :elapsed) 60)
                   (= (getf snapshot :worked) 180)
                   (= (getf snapshot :live-tasks) 2)
                   (= (getf snapshot :active-tasks) 1)
                   (= (getf snapshot :queue) 3)
                   (equal (getf snapshot :step) "checking changes")
                   (eq (getf snapshot :eta) ':unknown))
              "status reads the running activity and reports no invented ETA")
             (test-assert
              (equal before
                     (list (terminal-ui-status-started-at ui)
                           (terminal-ui-status-progress-at ui)
                           (conversation-next-sequence
                            (application-conversation application))))
              "reading status does not restart, advance, or stop active work"))
           (setf (terminal-ui-status ui) nil)
           (let ((snapshot (application-side-status-snapshot application)))
             (test-assert
              (and (= (getf snapshot :elapsed) 60)
                   (zerop (getf snapshot :worked))
                   (= (getf snapshot :live-tasks) 2))
              "background task duration does not double-count primary working time"))
           (setf (terminal-ui-agent-activities ui) nil)
           (let ((snapshot (application-side-status-snapshot application)))
             (test-assert (and (zerop (getf snapshot :elapsed))
                               (zerop (getf snapshot :live-tasks)))
                          "idle status has no running activity duration or tasks")))
      (platform-delete-directory-tree *platform* root :validate t
                                      :if-does-not-exist        ':ignore)))
  nil)

(-> test-side-status-history-and-context () null)
(defun test-side-status-history-and-context ()
  "Keep native status output in replay, outside every rebuilt context."
  (multiple-value-bind (application root)
      (lisp-machine-tests--application)
    (let* ((ui            (application-ui application))
           (conversation  (application-conversation application))
           (configuration (application-configuration application)))
      (unwind-protect
           (progn
             (terminal-ui-start ui)
             (application-run-lisp-input application "(list 1 2)" :interactive-p nil)
             (let ((operations (conversation-user-operation-snapshot conversation))
                   (evidence   (side-status-tests--evidence configuration conversation))
                   (items      (mapcar #'json-encode (conversation-input-items conversation))))
                (dolist (source '("(btw)" "(side :eta)" "(btw :focus)" "(side :queue)"))
                 (test-assert
                  (eq (application-run-lisp-input application source :interactive-p nil)
                      ':continue)
                  "Lisp status executes locally"))
               (let ((records (side-status-tests--records conversation)))
                 (test-assert (= (length records) 4)
                              "each status call stores exactly one display record")
                 (test-assert
                  (every (lambda (record)
                           (let ((spans (conversation-record-entry application record)))
                             (and (some (lambda (span)
                                          (eq (terminal-span-style span) ':notice))
                                        spans)
                                  (some (lambda (span)
                                          (not (eq (terminal-span-style span) ':plain)))
                                        spans))))
                         records)
                  "durable status output retains semantic colors"))
               (application-run-lisp-input application "(btw :invalid)" :interactive-p nil)
               (application-run-lisp-input application "(side :invalid)")
               (test-assert
                (and (= (length operations) 1)
                     (equal operations (conversation-user-operation-snapshot conversation))
                     (equal evidence (side-status-tests--evidence configuration conversation))
                     (equal items (mapcar #'json-encode (conversation-input-items conversation))))
                "success, failure, and abort never enter operation or provider context")
               (let ((loaded (conversation-load (conversation-pathname conversation))))
                 (test-assert (equal (side-status-tests--records conversation)
                                     (side-status-tests--records loaded))
                              "reload preserves each recorded status display and error")
                (test-assert
                 (and (equal operations (conversation-user-operation-snapshot loaded))
                      (equal evidence (side-status-tests--evidence configuration loaded))
                      (equal items (mapcar #'json-encode (conversation-input-items loaded))))
                 "reloading status displays does not rebuild them into context")
                (let* ((record    (first (side-status-tests--records loaded)))
                       (projected (conversation-replay-project-record record)))
                  (test-assert
                   (equal (getf (rest projected) :content)
                          (terminal--spans-text
                           (conversation-record-entry application record)))
                   "CLI replay and terminal replay share the durable status output")))))
        (terminal-ui-stop ui)
        (platform-delete-directory-tree *platform* root :validate t
                                        :if-does-not-exist        ':ignore))))
  nil)

(-> test-side-status-computed-arguments () null)
(defun test-side-status-computed-arguments ()
  "Retain ordinary Lisp effects when a status selector requires evaluation."
  (multiple-value-bind (application root)
      (lisp-machine-tests--application)
    (unwind-protect
         (let ((conversation (application-conversation application)))
           (dolist (case '(("(btw (progn (write-line \"computed selector output\") :focus))"
                            :continue :ok)
                           ("(side (progn (write-line \"computed selector output\") :invalid))"
                            :aborted :aborted)))
             (destructuring-bind (source action status) case
               (test-assert
                (and (not (application-side-status-source-p application source))
                     (eq (application-operation-source-active-turn-action application source)
                         ':hold))
                "computed selectors use the serialized ordinary Lisp path")
               (test-assert
                (eq (application-run-lisp-input application source :interactive-p nil) action)
                "computed selectors execute with the ordinary Lisp outcome")
               (let ((record (first (last (conversation-user-operation-snapshot conversation)))))
                 (test-assert
                  (and (equal (getf (rest record) :source) source)
                       (eq (getf (rest record) :status) status)
                       (search "computed selector output" (getf (rest record) :result)))
                  "argument output and outcome are retained with their Lisp source"))))
           (test-assert (= (length (side-status-tests--records conversation)) 1)
                        "the successful computed call also retains its display-only status")
           (test-assert
            (equal (conversation-user-operation-snapshot conversation)
                   (conversation-user-operation-snapshot
                    (conversation-load (conversation-pathname conversation))))
            "computed argument effects survive conversation reload"))
      (platform-delete-directory-tree *platform* root :validate t
                                      :if-does-not-exist        ':ignore)))
  nil)

(-> test-side-status-policies-and-validation () null)
(defun test-side-status-policies-and-validation ()
  "Resolve native aliases and busy policy, and reject corrupt display records."
  (multiple-value-bind (application root)
      (lisp-machine-tests--application)
    (unwind-protect
         (progn
           (dolist (source '("(btw)" "(side :eta)" "(btw :queue)"))
             (test-assert
              (and (application-side-status-source-p application source)
                   (eq (application-operation-source-active-turn-action application source)
                       ':execute))
              "native status forms use the registered immediate command policy"))
           (let* ((conversation (application-conversation application))
                  (record       (conversation-append-record
                                 conversation '(:side-status :source "(btw)"
                                                :spans ((:not-a-style . "invalid"))))))
             (declare (ignore record))
             (test-assert
              (handler-case
                  (progn (conversation-load (conversation-pathname conversation)) nil)
                (conversation-invariant-error ()
                  t))
              "invalid persisted styles are rejected at replay")))
      (platform-delete-directory-tree *platform* root :validate t
                                      :if-does-not-exist        ':ignore)))
  nil)

(-> test-side-status-message-jumps () null)
(defun test-side-status-message-jumps ()
  "Navigate to durable side output in both directions after replay."
  (multiple-value-bind (application root)
      (lisp-machine-tests--application)
    (unwind-protect
         (progn
           (setf (application-ui application) nil)
           (application-run-lisp-input application "(btw)" :interactive-p nil)
           (let* ((loaded   (conversation-load
                             (conversation-pathname (application-conversation application))))
                  (entry    (conversation-record-entry
                             application (first (side-status-tests--records loaded))))
                  (terminal (make-instance 'recording-terminal :columns 50 :rows 12))
                  (ui       (terminal-ui-create :terminal terminal :fullscreen-p t
                                                :message-header-prefixes
                                                *application-message-header-prefixes*)))
             (with-terminal-ui (active ui)
                (terminal-ui-present-records
                 active
                 (loop for index below 4
                       append (list (list (list :user index)
                                          (format nil "❯ you~%question ~D" index)
                                          '(:entry-kind :message))
                                    (list (list :status index) entry '(:entry-kind :side-status))
                                    (list (list :agent index)
                                          (format nil "● autolith~%answer ~D" index)
                                          '(:entry-kind :message)))))
               (terminal-ui-process-event active ':scroll-bottom)
               (let ((tail-top (fullscreen-test--top active)))
                 (terminal-ui-process-event active ':previous-section)
                 (let* ((viewport (fullscreen-terminal-ui-viewport active))
                        (top      (fullscreen-test--top active))
                        (height   (clinedi:transcript-viewport-height viewport)))
                   (test-assert
                    (and top
                         (loop for row from top below (+ top height)
                               thereis (uiop:string-prefix-p
                                        "◆ status" (fullscreen-test--row-text active row))))
                    "Ctrl-PgUp reaches replayed side output as the preceding message")
                   (terminal-ui-process-event active ':previous-section)
                   (terminal-ui-process-event active ':next-section)
                   (test-assert (eql top (fullscreen-test--top active))
                                "Ctrl-PgDn returns to the replayed side-output message"))
                 (terminal-ui-process-event active ':next-section)
                (terminal-ui-process-event active ':next-section)
                (test-assert (eql tail-top (fullscreen-test--top active))
                             "jumping past the newest assistant message resumes tail following")))))
      (platform-delete-directory-tree *platform* root :validate t
                                      :if-does-not-exist        ':ignore)))
  nil)
