(in-package #:autolith)

;;;; -- Recovery Input Vault Test Support --

(-> recovery-input-vault-tests--application
    (configuration string)
    application)
(defun recovery-input-vault-tests--application (configuration identifier)
  "Return a minimal application for one durable test conversation."
  (let ((conversation
          (conversation-create configuration :identifier identifier)))
    (conversation-append-user-message conversation "seed")
    (make-instance 'application
                   :configuration configuration
                   :conversation  conversation)))

(-> test-recovery-input-vault-corruption () null)
(defun test-recovery-input-vault-corruption ()
  "Test malformed vault state is rejected without changing its bytes."
  (with-test-configuration (configuration)
    (let* ((application  (recovery-input-vault-tests--application configuration "recovery-vault-corrupt"))
           (conversation (application-conversation application))
           (pathname     (configuration-recovery-input-vault-path
                          configuration (conversation-pathname conversation)))
           (vault-form   (list :recovery-input-vault :version 1
                               :conversation-id      (conversation-identifier conversation)
                               :captures             (list (list :id "schema-capture" :captured-at 0
                                                                 :active-work nil :steering-in-flight nil
                                                                 :steering nil
                                                                 :work     '((:message "vault payload")))))))
      (ensure-directories-exist pathname)
      (with-open-file (stream pathname :direction ':output :if-exists ':supersede
                                       :external-format ':utf-8)
        (write-string "(:recovery-input-vault :version 1" stream))
      (test-assert
       (handler-case (progn (application-recovery-input-vault-captures application) nil)
         (recovery-input-vault-error () t))
       "strict vault inspection reports truncated data")
      (dolist (mutate (list (lambda (form) (setf (first form) ':foreign-vault))
                            (lambda (form) (setf (getf (rest form) :version) 2))
                            (lambda (form) (nconc form (list :version 1)))
                            (lambda (form) (nconc form (list :unexpected t)))
                            (lambda (form) (remf (rest form) :captures))
                            (lambda (form) (nconc (first (getf (rest form) :captures))
                                                  (list :version 1)))
                            (lambda (form)
                              (let ((capture (first (getf (rest form) :captures))))
                                (nconc capture (list :id (getf capture :id)))))))
        (let ((form (copy-tree vault-form)))
          (funcall mutate form)
          (snapshot-write pathname form :mode #o600)
          (let ((octets (read-file-into-byte-vector pathname)))
            (test-assert
             (handler-case (progn (application-recovery-input-vault-captures application) nil)
               (recovery-input-vault-error (condition)
                 (and (eq (recovery-input-vault-error-operation condition) ':validate-vault)
                      (equalp octets (read-file-into-byte-vector pathname)))))
             "strict vault inspection rejects invalid schemas without changing bytes"))))))
  nil)


;;;; -- Vault Controls --

(-> recovery-input-vault-tests--vault-form
    (conversation list)
    list)
(defun recovery-input-vault-tests--vault-form (conversation captures)
  "Return one complete test vault form for CONVERSATION and CAPTURES."
  (list :recovery-input-vault
        :version 1
        :conversation-id (conversation-identifier conversation)
        :captures        captures))

(-> test-recovery-input-vault-restore () null)
(defun test-recovery-input-vault-restore ()
  "Test restore order, image preservation, recalled FIFO, and steering barriers."
  (let* ((configuration  (test-configuration))
         (root           (test-configuration-root configuration))
         (terminal       (make-instance 'waiting-recording-terminal :columns 100))
         (ui             (terminal-ui-create :terminal terminal))
         (conversation
           (conversation-create configuration :identifier "recovery-vault-restore"))
         (application
           (make-instance 'application
                          :configuration configuration
                          :conversation  conversation
                          :ui            ui))
         (image-pathname (merge-pathnames "queued-image.png" root))
         (image-input
            (user-message-input-create
             :text            ""
             :image-pathnames (list image-pathname)
             :submission      '(:id "input" :client "editor" :source "queue")))
         (vault-pathname
           (configuration-recovery-input-vault-path
            configuration (conversation-pathname conversation)))
         (pending-pathname
           (configuration-pending-inputs-path
            configuration (conversation-pathname conversation)))
         (controller     nil))
    (unwind-protect
         (progn
           (configuration-ensure-directories configuration)
           (conversation-append-user-message conversation "seed")
           (terminal-ui-start ui)
           (setf controller
                 (application-input-controller-create
                  application :load-pending-p nil))
           (ensure-directories-exist vault-pathname)
           (snapshot-write
            vault-pathname
            (recovery-input-vault-tests--vault-form
             conversation
             (list
              (list :id          "older-capture"
                    :captured-at 10
                    :active-work
                    (list :identifier "active-image"
                          :work
                          (list ':message
                                (input-value->form image-input)))
                    :steering-in-flight
                    '((:identifier "flight" :input "in-flight steering"))
                    :steering    '("queued steering")
                    :work        '((:message "old follow-up")))
              (list :id                 "newer-capture"
                    :captured-at        20
                    :active-work        nil
                    :steering-in-flight nil
                    :steering           nil
                    :work               '((:lisp "(status)")))))
            :mode #o600)
           (recording-terminal-reset terminal)
           (application-operation-call application 'vault)
           (let ((output (recording-terminal-output terminal)))
             (test-assert
              (and (search "recovery input vault" output)
                   (search "old follow-up" output)
                   (search "(status)" output))
              "/vault lists chronological captures with bounded previews"))
           (with-lock-held ((application-input-controller-lock controller))
             (setf (application-input-controller-active-p controller) t
                   (application-input-controller-active-work-kind controller)
                   ':message))
           (application-input-controller--enqueue
            controller ':message "current recalled")
           (test-assert
            (application-input-controller--recall-follow-up controller)
            "restore test holds one newer recalled follow-up")
           (application-operation-call application 'vault-restore)
           (test-assert
            (and (not (probe-file vault-pathname))
                 (= (application-input-controller-steering-promotion-prefix-count
                     controller)
                    5)
                 (= (application-input-controller-follow-up-edit-index controller)
                    5))
            "restore publishes five older items and retains recalled FIFO position")
           (application-input-controller-submit-primary-prompt
            controller "typed during restore")
           (multiple-value-bind (pending-form complete-p)
               (snapshot-read pending-pathname)
             (let* ((state
                      (and complete-p
                           (input-state-from-form
                            pending-form (conversation-identifier conversation))))
                    (filtered
                      (and state
                           (input-state-filter-persisted
                            state conversation)))
                    (work (and filtered (getf filtered :work-items))))
               (test-assert
                (and complete-p
                     (= (length work) 7)
                     (typep (second (first work)) 'user-message-input)
                     (equal (user-message-input-image-pathnames
                             (second (first work)))
                            (list image-pathname))
                     (equal (user-message-input-submission (second (first work)))
                            (user-message-input-submission image-input))
                     (string= (second (second work)) "in-flight steering")
                     (string= (second (third work)) "queued steering")
                     (string= (second (fourth work)) "old follow-up")
                     (equal (fifth work) '(:lisp "(status)"))
                     (string= (second (sixth work)) "typed during restore")
                     (string= (second (seventh work)) "current recalled"))
                "a crash during restore preserves older input before new steering and recalled work")))
           (application-input-controller--finish-work controller)
           (with-lock-held ((application-input-controller-lock controller))
             (let ((work
                     (application-input-controller--virtual-work-items controller)))
               (test-assert
                (and (= (length work) 7)
                     (= (application-input-controller-steering-promotion-prefix-count
                         controller)
                        6)
                     (= (application-input-controller-follow-up-edit-index
                         controller)
                        6)
                     (typep (second (first work)) 'user-message-input)
                     (string= (second (sixth work)) "typed during restore")
                     (string= (second (seventh work)) "current recalled"))
                "finishing restore promotes newer steering and retains its durable prefix"))))
      (when controller
        (ignore-errors (application-input-controller-stop controller)))
      (ignore-errors (terminal-ui-stop ui))
      (platform-delete-directory-tree *platform* root :validate t :if-does-not-exist ':ignore)))
  nil)

(-> test-recovery-input-vault-active-restore-crash () null)
(defun test-recovery-input-vault-active-restore-crash ()
  "Test a crash during restored active work preserves its steering barrier."
  (let* ((configuration (test-configuration))
         (root          (test-configuration-root configuration))
         (terminal      (make-instance 'waiting-recording-terminal :columns 80))
         (ui            (terminal-ui-create :terminal terminal))
         (conversation
           (conversation-create
            configuration :identifier "recovery-vault-active-restore-crash"))
         (application
           (make-instance 'application
                          :configuration configuration
                          :conversation  conversation
                          :ui            ui))
         (vault-pathname
           (configuration-recovery-input-vault-path
            configuration (conversation-pathname conversation)))
         (controller    nil))
    (unwind-protect
         (progn
           (configuration-ensure-directories configuration)
           (conversation-append-user-message conversation "seed")
           (terminal-ui-start ui)
           (setf controller
                 (application-input-controller-create
                  application :load-pending-p nil))
           (ensure-directories-exist vault-pathname)
           (snapshot-write
            vault-pathname
            (recovery-input-vault-tests--vault-form
             conversation
             (list
              (list :id                 "active-restore-capture"
                    :captured-at        10
                    :active-work        nil
                    :steering-in-flight nil
                    :steering           nil
                    :work               '((:message "restored active")
                                          (:message "restored queued")))))
            :mode #o600)
           (test-assert
            (= (application-input-controller-vault-restore controller) 2)
            "two recovered messages enter the restore ordering barrier")
           (test-assert
            (equal (application-input-controller--next-work controller)
                   '(:message "restored active"))
            "the first restored message becomes active")
           (application-input-controller-submit-primary-prompt
            controller "late steering")
           (application-input-controller--enqueue
            controller ':message "newer follow-up")
           (let* ((pending (configuration-pending-inputs-path
                            configuration (conversation-pathname conversation)))
                  (state   (input-state-from-form (snapshot-read pending)
                                                  (conversation-identifier conversation))))
             (application-recovery-input-vault--write-captures
              application (list (application-recovery-input-vault--capture-state
                                 state (get-universal-time))))
             (delete-file pending))
           (let* ((captures
                    (application-recovery-input-vault-captures application))
                  (capture (first captures))
                  (ordered-work
                    (and capture
                         (application-recovery-input-vault--capture-work capture))))
             (test-assert
              (and (= (length captures) 1)
                   (= (getf capture :steering-promotion-prefix-count) 1)
                   (equal ordered-work
                          '((:message "restored active")
                            (:message "restored queued")
                            (:message "late steering")
                            (:message "newer follow-up"))))
              "vault capture keeps older restored work ahead of later steering"))
           (application-input-controller-stop controller)
           (setf controller nil)
           (setf controller
                 (application-input-controller-create
                  application :load-pending-p nil))
           (test-assert
            (= (application-input-controller-vault-restore controller) 4)
            "the active crash capture restores all four inputs")
           (test-assert
            (equal (application-input-controller--state controller :work-items)
                   '((:message "restored active")
                     (:message "restored queued")
                     (:message "late steering")
                     (:message "newer follow-up")))
            "restoring the active crash capture preserves its exact input order"))
      (when controller
        (ignore-errors (application-input-controller-stop controller)))
      (ignore-errors (terminal-ui-stop ui))
      (platform-delete-directory-tree *platform* root :validate t :if-does-not-exist ':ignore)))
  nil)

(-> test-recovery-input-vault-restore-rollback () null)
(defun test-recovery-input-vault-restore-rollback ()
  "Test failed vault deletion exposes provenance and restores prior controller state."
  (let* ((configuration         (test-configuration))
         (root                  (test-configuration-root configuration))
         (terminal              (make-instance 'waiting-recording-terminal :columns 80))
         (ui                    (terminal-ui-create :terminal terminal))
         (conversation
           (conversation-create configuration :identifier "recovery-vault-rollback"))
         (application
           (make-instance 'application
                          :configuration configuration
                          :conversation  conversation
                          :ui            ui))
         (vault-pathname
           (configuration-recovery-input-vault-path
            configuration (conversation-pathname conversation)))
         (pending-pathname
           (configuration-pending-inputs-path
            configuration (conversation-pathname conversation)))
         (controller            nil)
         (original-writer
           (symbol-function 'application-recovery-input-vault--write-captures))
         (provenance-observed-p nil))
    (unwind-protect
         (progn
           (configuration-ensure-directories configuration)
           (conversation-append-user-message conversation "seed")
           (terminal-ui-start ui)
           (setf controller
                 (application-input-controller-create
                  application :load-pending-p nil))
           (application-input-controller--enqueue
            controller ':message "existing work")
           (ensure-directories-exist vault-pathname)
           (snapshot-write
            vault-pathname
            (recovery-input-vault-tests--vault-form
             conversation
             (list
              (list :id                 "rollback-capture"
                    :captured-at        10
                    :active-work        nil
                    :steering-in-flight nil
                    :steering           nil
                    :work               '((:message "restored work")))))
            :mode #o600)
           (setf
            (symbol-function 'application-recovery-input-vault--write-captures)
            (lambda (writer-application captures)
              (if captures
                  (funcall original-writer writer-application captures)
                  (multiple-value-bind (form complete-p)
                      (snapshot-read pending-pathname)
                    (setf provenance-observed-p
                          (and complete-p
                               (equal
                                (getf (rest form) :vault-capture-identifiers)
                                '("rollback-capture"))))
                    (error "forced vault deletion failure")))))
           (test-assert
            (handler-case
                (progn
                  (application-input-controller-vault-restore controller)
                  nil)
              (recovery-input-vault-error ()
                t))
            "restore reports a typed failure when vault deletion fails")
           (test-assert provenance-observed-p
                        "restore publishes capture provenance before deleting the vault")
           (test-assert
            (and (probe-file vault-pathname)
                 (equal (application-input-controller--state controller :work-items)
                        '((:message "restored work")
                          (:message "existing work"))))
            "failed vault deletion keeps committed restored and concurrent queue state")
           (multiple-value-bind (form complete-p)
               (snapshot-read pending-pathname)
             (test-assert
              (and complete-p
                   (equal (getf (rest form) :vault-capture-identifiers)
                          '("rollback-capture"))
                   (equal (getf (rest form) :work)
                          '((:message "restored work")
                            (:message "existing work"))))
              "failed deletion preserves one provenance-bearing pending snapshot")))
      (setf (symbol-function 'application-recovery-input-vault--write-captures)
            original-writer)
      (when controller
        (ignore-errors (application-input-controller-stop controller)))
      (ignore-errors (terminal-ui-stop ui))
      (platform-delete-directory-tree *platform* root :validate t :if-does-not-exist ':ignore)))
  nil)

(-> test-recovery-input-vault-post-delete-rollback () null)
(defun test-recovery-input-vault-post-delete-rollback ()
  "Test a failed final publication preserves the provenance snapshot."
  (let* ((configuration     (test-configuration))
         (root              (test-configuration-root configuration))
         (terminal          (make-instance 'waiting-recording-terminal :columns 80))
         (ui                (terminal-ui-create :terminal terminal))
         (conversation
           (conversation-create
            configuration :identifier "recovery-vault-post-delete-rollback"))
         (application
           (make-instance 'application
                          :configuration configuration
                          :conversation  conversation
                          :ui            ui))
         (vault-pathname
           (configuration-recovery-input-vault-path
            configuration (conversation-pathname conversation)))
         (pending-pathname
           (configuration-pending-inputs-path
            configuration (conversation-pathname conversation)))
         (controller        nil)
         (publication-count 0)
         (original-publisher
           (symbol-function
            'application-input-controller--publish-pending-publication-locked)))
    (unwind-protect
         (progn
           (configuration-ensure-directories configuration)
           (conversation-append-user-message conversation "seed")
           (terminal-ui-start ui)
           (setf controller
                 (application-input-controller-create
                  application :load-pending-p nil))
           (application-input-controller--enqueue
            controller ':message "existing work")
           (ensure-directories-exist vault-pathname)
           (snapshot-write
            vault-pathname
            (recovery-input-vault-tests--vault-form
             conversation
             (list
              (list :id                 "post-delete-capture"
                    :captured-at        10
                    :active-work        nil
                    :steering-in-flight nil
                    :steering           nil
                    :work               '((:message "restored work")))))
            :mode #o600)
           (setf
            (symbol-function
             'application-input-controller--publish-pending-publication-locked)
            (lambda (writer-controller publication error-p)
              (incf publication-count)
              (if (= publication-count 2)
                  (error "forced final pending publication failure")
                  (funcall original-publisher
                           writer-controller publication error-p))))
           (test-assert
            (handler-case
                (progn
                  (application-input-controller-vault-restore controller)
                  nil)
              (recovery-input-vault-error ()
                t))
            "restore reports a typed failure after vault deletion")
           (test-assert
            (and (= publication-count 2)
                 (not (probe-file vault-pathname))
                 (application-input-controller-pending-persistence-enabled-p
                  controller))
            "failed final publication leaves ingress enabled and the vault absent")
           (multiple-value-bind (form complete-p)
               (snapshot-read pending-pathname)
             (test-assert
              (and complete-p
                   (equal (getf (rest form) :vault-capture-identifiers)
                          '("post-delete-capture"))
                   (equal (getf (rest form) :work)
                          '((:message "restored work")
                            (:message "existing work"))))
              "failed final publication retains restored work with provenance")))
      (setf
       (symbol-function
        'application-input-controller--publish-pending-publication-locked)
       original-publisher)
      (when controller
        (ignore-errors (application-input-controller-stop controller)))
      (ignore-errors (terminal-ui-stop ui))
      (platform-delete-directory-tree *platform* root :validate t :if-does-not-exist ':ignore)))
  nil)

(-> test-recovery-input-vault-discard () null)
(defun test-recovery-input-vault-discard ()
  "Test discard removes only exact blocked state and re-enables persistence."
  (let* ((configuration (test-configuration))
         (root          (test-configuration-root configuration))
         (terminal      (make-instance 'waiting-recording-terminal :columns 80))
         (ui            (terminal-ui-create :terminal terminal))
         (conversation
           (conversation-create configuration :identifier "recovery-vault-discard"))
         (other-conversation
           (conversation-create configuration :identifier "recovery-vault-other"))
         (application
           (make-instance 'application
                          :configuration configuration
                          :conversation  conversation
                          :ui            ui))
         (current-vault
           (configuration-recovery-input-vault-path
            configuration (conversation-pathname conversation)))
         (other-vault
           (configuration-recovery-input-vault-path
            configuration (conversation-pathname other-conversation)))
         (pending-pathname
           (configuration-pending-inputs-path
            configuration (conversation-pathname conversation)))
         (legacy-pathname
           (configuration-legacy-pending-inputs-path configuration))
         (legacy-octets nil)
         (controller    nil))
    (unwind-protect
         (progn
           (configuration-ensure-directories configuration)
           (conversation-append-user-message conversation "seed")
           (conversation-append-user-message other-conversation "seed")
           (ensure-directories-exist current-vault)
           (with-open-file (stream current-vault
                                   :direction         ':output
                                   :if-exists         ':supersede
                                   :if-does-not-exist ':create
                                   :external-format   ':utf-8)
             (write-string "(:corrupt" stream))
           (ensure-directories-exist other-vault)
           (snapshot-write
            other-vault
            (recovery-input-vault-tests--vault-form
             other-conversation
             (list
              (list :id                 "other-capture"
                    :captured-at        10
                    :active-work        nil
                    :steering-in-flight nil
                    :steering           nil
                    :work               '((:message "other work")))))
            :mode #o600)
           (ensure-directories-exist pending-pathname)
           (snapshot-write
            pending-pathname
            (list :pending-inputs
                  :version 2
                  :snapshot-identifier             "discard-pending"
                  :conversation-id                 (conversation-identifier conversation)
                  :active-work                     nil
                  :steering-in-flight              nil
                  :steering                        nil
                  :work                            '((:message "discard me"))
                  :vault-capture-identifiers       nil
                  :steering-promotion-prefix-count 0)
            :mode #o600)
           (ensure-directories-exist legacy-pathname)
           (with-open-file (stream legacy-pathname
                                   :direction         ':output
                                   :if-exists         ':supersede
                                   :if-does-not-exist ':create
                                   :external-format   ':utf-8)
             (write-string "(:pending-inputs :version" stream))
           (setf legacy-octets
                 (read-file-into-byte-vector legacy-pathname))
           (terminal-ui-start ui)
           (setf controller
                 (application-input-controller-create
                  application
                  :load-pending-p                nil
                  :pending-persistence-enabled-p nil)
                 (application-recovery-input-vault-failure application)
                 (make-condition
                  'recovery-input-vault-error
                  :message   "corrupt recovery state"
                  :pathname  current-vault
                  :operation ':read-vault))
           (recording-terminal-reset terminal)
           (application-operation-call application 'vault-discard)
           (test-assert
            (and (search "discarded" (recording-terminal-output terminal))
                 (not (probe-file current-vault))
                 (not (probe-file pending-pathname))
                 (probe-file other-vault)
                 (probe-file legacy-pathname)
                 (equalp legacy-octets
                         (read-file-into-byte-vector legacy-pathname))
                 (application-input-controller-pending-persistence-enabled-p
                  controller)
                 (null (application-recovery-input-vault-failure application)))
            "/vault-discard removes only attributed state and preserves corrupt global legacy bytes"))
      (when controller
        (ignore-errors (application-input-controller-stop controller)))
      (ignore-errors (terminal-ui-stop ui))
      (platform-delete-directory-tree *platform* root :validate t :if-does-not-exist ':ignore)))
  nil)

(-> test-recovery-input-vault-disabled-ingress () null)
(defun test-recovery-input-vault-disabled-ingress ()
  "Test blocked recovery storage admits only the exact vault controls."
  (let* ((configuration (test-configuration))
         (root          (test-configuration-root configuration))
         (terminal      (make-instance 'waiting-recording-terminal :columns 80))
         (ui            (terminal-ui-create :terminal terminal))
         (conversation
           (conversation-create configuration :identifier "recovery-vault-ingress"))
         (application
           (make-instance 'application
                          :configuration configuration
                          :conversation  conversation
                          :ui            ui))
         (controller    nil))
    (unwind-protect
         (progn
           (configuration-ensure-directories configuration)
           (conversation-append-user-message conversation "seed")
           (terminal-ui-start ui)
           (setf controller
                 (application-input-controller-create
                  application
                  :load-pending-p                nil
                  :pending-persistence-enabled-p nil))
           (recording-terminal-reset terminal)
           (application-input-controller--handle-submission
            controller "ordinary message")
           (application-input-controller--handle-submission controller "(status)")
           (test-assert
            (and (null (application-input-controller--state controller :work-items))
                 (null (application-input-controller--state
                        controller :steering-items))
                 (search "recovered input storage is unavailable"
                         (recording-terminal-output terminal)))
            "disabled recovery storage rejects ordinary messages and commands")
           (test-assert
            (every
             (lambda (command)
               (application-input-controller--submission-storage-ready-p
                controller command))
             '("(vault)" "(vault-restore)" "(vault-discard)" "(vault-store)"))
            "disabled recovery storage admits every exact vault control")
           (application-input-controller--handle-submission controller "(vault)")
           (test-assert
            (equal (application-input-controller--state controller :work-items)
                   '((:lisp "(vault)")))
            "an admitted vault control reaches normal command scheduling"))
      (when controller
        (ignore-errors (application-input-controller-stop controller)))
      (ignore-errors (terminal-ui-stop ui))
      (platform-delete-directory-tree *platform* root :validate t :if-does-not-exist ':ignore)))
  nil)

(-> test-recovery-input-vault-disabled-recalled-ingress () null)
(defun test-recovery-input-vault-disabled-recalled-ingress ()
  "Test recalled Enter input cannot bypass blocked recovery storage."
  (let* ((configuration (test-configuration))
         (root          (test-configuration-root configuration))
         (terminal      (make-instance 'waiting-recording-terminal :columns 80))
         (ui            (terminal-ui-create :terminal terminal))
         (conversation
           (conversation-create
            configuration :identifier "recovery-vault-recalled-ingress"))
         (application
           (make-instance 'application
                          :configuration configuration
                          :conversation  conversation
                          :ui            ui))
         (pending-pathname
           (configuration-pending-inputs-path
            configuration (conversation-pathname conversation)))
         (controller    nil))
    (unwind-protect
         (progn
           (configuration-ensure-directories configuration)
           (conversation-append-user-message conversation "seed")
           (terminal-ui-start ui)
           (setf controller
                 (application-input-controller-create
                  application :load-pending-p nil))
           (with-lock-held ((application-input-controller-lock controller))
             (setf (application-input-controller-active-p controller) t))
           (application-input-controller--enqueue
            controller ':message "held follow-up")
           (test-assert
            (application-input-controller--recall-follow-up controller)
            "the follow-up is recalled before storage becomes blocked")
           (terminal-ui-set-input ui "edited held follow-up")
           (setf (application-input-controller-pending-persistence-enabled-p
                  controller)
                 nil)
           (recording-terminal-reset terminal)
           (application-input-controller--process-event controller ':submit)
           (test-assert
            (and (application-input-controller--follow-up-editing-p controller)
                 (= (application-input-controller-follow-up-edit-index controller) 0)
                 (equal (application-input-controller-follow-up-edit-work controller)
                        '(:message "held follow-up"))
                 (null (application-input-controller--state controller :work-items))
                 (null (application-input-controller--state
                        controller :steering-items))
                 (search "recovered input storage is unavailable"
                         (recording-terminal-output terminal)))
            "blocked recalled submission leaves the durable held follow-up selected")
           (multiple-value-bind (form complete-p)
               (snapshot-read pending-pathname)
             (test-assert
              (and complete-p
                   (equal (getf (rest form) :work)
                          '((:message "held follow-up"))))
              "blocked recalled submission leaves its prior pending snapshot intact")))
      (when controller
        (ignore-errors (application-input-controller-stop controller)))
      (ignore-errors (terminal-ui-stop ui))
      (platform-delete-directory-tree *platform* root :validate t :if-does-not-exist ':ignore)))
  nil)


;;;; -- Startup Integration --

(-> recovery-input-vault-tests--startup-application
    (configuration string terminal)
    application)
(defun recovery-input-vault-tests--startup-application
    (configuration identifier terminal)
  "Return a complete minimal startup application for one test conversation."
  (let ((conversation
          (conversation-create configuration :identifier identifier)))
    (conversation-append-user-message conversation "seed")
    (make-instance 'application
                   :configuration configuration
                   :conversation  conversation
                   :provider      nil
                   :tool-registry (make-instance 'tool-registry)
                   :worker        nil
                   :agent         nil
                   :ui            (terminal-ui-create :terminal terminal))))

(-> recovery-input-vault-tests--serve-startup
    (application &key (:initial-input (option user-message-input)))
    null)
(defun recovery-input-vault-tests--serve-startup
    (application &key initial-input)
  "Serve the prepared vault application as an owned world host through startup and shutdown."
  (when initial-input
    (terminal-ui-set-input (application-ui application) initial-input))
  (world-server-tests--call-with-server
   (application-configuration application)
   (lambda (server)
     (test-call-with-function-replacements
      (list (list 'application--build
                  (lambda (&rest arguments)
                    (declare (ignore arguments))
                    application)))
      (lambda ()
        (let ((host (world-server--build-host
                     server
                     :conversation-id (conversation-identifier
                                       (application-conversation application))
                     :directory       (namestring (config :working-directory
                                                         (application-configuration application)))
                     :arguments       (list :switch-p t))))
          (test-assert
           (task-tests--wait-until
            (lambda () (not (thread-alive-p (world-host-thread host)))) 30)
           "the owned vault startup host finishes its scheduler and cleanup")
          (when (world-host-failure host)
            (error (world-host-failure host))))))))
  nil)

(-> test-recovery-input-vault-startup-notice () null)
(defun test-recovery-input-vault-startup-notice ()
  "Test startup notice message counts, severity, and Lisp-form controls."
  (with-test-configuration (configuration)
    (let ((application
            (recovery-input-vault-tests--application
             configuration "recovery-vault-startup-notice"))
          (captures nil)
          (entries  nil))
      (test-call-with-function-replacements
       (list
        (list 'application-recovery-input-vault-captures
              (lambda (run-application)
                (declare (ignore run-application))
                captures))
        (list 'application-present
              (lambda (run-application entry)
                (declare (ignore run-application))
                (push entry entries)
                nil)))
       (lambda ()
         (test-assert
          (and (null (application-recovery-input-vault-present-startup-warning
                      application))
               (null entries))
          "an empty vault produces no startup notice")
         (dolist (case '((1 ((:work-items ((:message "one")))))
                         (2 ((:work-items ((:message "one") (:message "two")))))
                         (5 ((:active-work    (:message "active")
                              :steering-items ("steer")
                              :work-items     ((:message "follow-up")))
                             (:work-items ((:message "four") (:message "five")))))))
           (setf captures (second case))
           (test-assert
            (application-recovery-input-vault-present-startup-warning application)
            "saved input produces a startup notice")
           (let* ((entry   (first entries))
                  (summary (first entry))
                  (controls
                    (remove-if-not
                     (lambda (span) (eq (terminal-span-style span) ':code))
                     entry)))
             (test-assert
              (and (eq (terminal-span-style summary) ':notice)
                   (= (parse-integer
                       (terminal-span-text summary)
                       :start        (position-if #'digit-char-p (terminal-span-text summary))
                       :junk-allowed t)
                      (first case)))
              "notice severity reports message totals rather than capture totals")
             (test-assert
              (and controls
                   (every (lambda (span)
                            (let ((*read-eval* nil))
                              (let ((form (read-from-string (terminal-span-text span))))
                                (and (consp form)
                                     (symbolp (first form))
                                     (null (rest form))))))
                          controls))
              "vault controls use code-styled Lisp calls")))
         (setf (application-recovery-input-vault-failure application)
               (make-condition 'recovery-input-vault-error
                               :message   "unreadable vault fixture"
                               :pathname  (test-configuration-root configuration)
                               :operation ':read-vault))
         (test-assert
          (application-recovery-input-vault-present-startup-warning application)
          "blocked recovery storage produces a startup notice")
         (test-assert
          (eq (terminal-span-style (first (first entries))) ':failure)
          "storage failure has failure severity")))))
  nil)

(-> test-recovery-input-vault-ordinary-startup () null)
(defun test-recovery-input-vault-ordinary-startup ()
  "Test ordinary startup retains normal pending-input loading."
  (let* ((configuration           (test-configuration))
         (root                    (test-configuration-root configuration))
         (terminal                (make-instance 'waiting-recording-terminal :columns 90))
         (application
           (recovery-input-vault-tests--startup-application
            configuration "recovery-vault-ordinary-startup" terminal))
         (conversation            (application-conversation application))
         (pending-pathname
           (configuration-pending-inputs-path
            configuration (conversation-pathname conversation)))
         (vault-pathname
           (configuration-recovery-input-vault-path
            configuration (conversation-pathname conversation)))
         (original-create
           (symbol-function 'application-input-controller-create))
         (observed-load-pending-p :unset)
         (observed-persistence-p  :unset)
         (observed-start-reader-p :unset)
         (observed-first-work     :unset)
         (observed-work           :unset)
         (observed-steering       :unset))
    (unwind-protect
         (progn
           (configuration-ensure-directories configuration)
           (ensure-directories-exist pending-pathname)
           (snapshot-write
            pending-pathname
            (list :pending-inputs
                  :version 2
                  :snapshot-identifier             "ordinary-startup-pending"
                  :conversation-id                 (conversation-identifier conversation)
                  :active-work                     nil
                  :steering-in-flight              nil
                  :steering                        '("ordinary steering")
                  :work                            '((:message "ordinary follow-up"))
                  :vault-capture-identifiers       nil
                  :steering-promotion-prefix-count 0)
            :mode #o600)
           (test-call-with-function-replacements
            (list
             (list
              'application-input-controller-create
               (lambda (run-application &rest options
                        &key (load-pending-p t) (pending-persistence-enabled-p t)
                             (start-reader-p t) &allow-other-keys)
                (setf observed-load-pending-p load-pending-p
                      observed-persistence-p pending-persistence-enabled-p
                      observed-start-reader-p start-reader-p)
                (apply original-create run-application options)))
             (list
              'application-input-controller--run-work
              (lambda (controller work)
                (setf observed-first-work (copy-tree work)
                      observed-work
                      (application-input-controller--state controller :work-items)
                      observed-steering
                      (application-input-controller--state controller :steering-items))
                (with-lock-held ((application-input-controller-lock controller))
                  (setf (application-input-controller-stopping-p controller) t)
                  (sb-thread:condition-broadcast
                   (application-input-controller-condition-variable controller)))
                nil)))
            (lambda ()
              (recovery-input-vault-tests--serve-startup
               application
               :initial-input (user-message-input-create :text "draft"))))
           (test-assert
            (and observed-load-pending-p
                 observed-persistence-p
                 (null observed-start-reader-p)
                 (equal observed-first-work
                        '(:message "ordinary steering"))
                 (equal observed-work
                        '((:message "ordinary follow-up")))
                 (null observed-steering))
            "ordinary startup schedules conversation-scoped pending input")
           (test-assert
            (and (probe-file pending-pathname)
                 (not (probe-file vault-pathname)))
            "ordinary startup does not vault pending input"))
      (platform-delete-directory-tree *platform* root :validate t :if-does-not-exist ':ignore)))
  nil)

(-> test-recovery-input-vault-live-primary-submit () null)
(defun test-recovery-input-vault-live-primary-submit ()
  "Test accepted primary follow-ups are vaulted immediately and removed when consumed."
  (let* ((configuration (test-configuration))
         (root          (test-configuration-root configuration))
         (terminal      (make-instance 'waiting-recording-terminal :columns 80))
         (ui            (terminal-ui-create :terminal terminal))
         (conversation
           (conversation-create configuration :identifier "recovery-vault-live-submit"))
         (application
           (make-instance 'application
                          :configuration configuration
                          :conversation  conversation
                          :ui            ui))
         (pending-pathname
           (configuration-pending-inputs-path
            configuration (conversation-pathname conversation)))
         (controller    nil))
    (unwind-protect
         (progn
           (configuration-ensure-directories configuration)
           (conversation-append-user-message conversation "seed")
           (terminal-ui-start ui)
           (setf controller
                 (application-input-controller-create
                  application
                  :load-pending-p nil
                  :start-reader-p nil))
           (multiple-value-bind (accepted-p delivery)
               (application-input-controller-submit-primary-prompt
                controller "live follow-up")
             (test-assert (and accepted-p (eq delivery ':queued))
                          "a primary follow-up is accepted as queued work"))
           (let* ((captures
                    (application-recovery-input-vault-captures application))
                  (capture (first captures)))
             (test-assert
              (and (probe-file pending-pathname)
                   (= (length captures) 1)
                   (equal (application-recovery-input-vault--capture-work capture)
                          '((:message "live follow-up"))))
              "accepted primary follow-up is persisted and vaulted immediately"))
           (with-lock-held ((application-input-controller-lock controller))
             (deque-clear (application-input-controller-work-items controller)))
           (application-input-controller--persist-pending controller)
           (test-assert
            (and (not (probe-file pending-pathname))
                 (null (application-recovery-input-vault-captures application)))
            "consuming accepted pending input removes the live vault capture"))
      (when controller
        (ignore-errors (application-input-controller-stop controller)))
      (ignore-errors (terminal-ui-stop ui))
      (platform-delete-directory-tree *platform* root :validate t :if-does-not-exist ':ignore)))
  nil)

(-> test-recovery-input-vault-capture-message () null)
(defun test-recovery-input-vault-capture-message ()
  "Test child-steer captures persist and survive reload."
  (let* ((configuration (test-configuration))
         (root          (test-configuration-root configuration))
         (application
           (recovery-input-vault-tests--application
            configuration "recovery-vault-capture-message"))
         (conversation  (application-conversation application))
         (identifier    nil))
    (unwind-protect
         (progn
           (configuration-ensure-directories configuration)
           (setf identifier
                 (application-recovery-input-vault-capture-message
                  application "child steer"))
           (let* ((captures
                    (application-recovery-input-vault-captures application))
                  (capture (first captures)))
             (test-assert
              (and (non-empty-string-p identifier)
                   (= (length captures) 1)
                   (string= (getf capture :id) identifier)
                   (equal (application-recovery-input-vault--capture-work capture)
                          '((:message "child steer"))))
              "a child-steer capture is written with the accepted message"))
           (let* ((reloaded
                    (make-instance 'application
                                   :configuration configuration
                                   :conversation  conversation))
                  (captures
                    (application-recovery-input-vault-captures reloaded))
                  (capture (first captures)))
             (test-assert
              (and (= (length captures) 1)
                   (string= (getf capture :id) identifier)
                   (equal (application-recovery-input-vault--capture-work capture)
                          '((:message "child steer"))))
              "a child-steer capture survives reload from disk")))
      (platform-delete-directory-tree *platform* root :validate t :if-does-not-exist ':ignore)))
  nil)

(-> test-recovery-input-vault-capture-during-restore () null)
(defun test-recovery-input-vault-capture-during-restore ()
  "Test a child capture cannot be deleted by a concurrent vault restore."
  (let* ((configuration                      (test-configuration))
         (root                               (test-configuration-root configuration))
         (terminal                           (make-instance 'waiting-recording-terminal :columns 80))
         (ui                                 (terminal-ui-create :terminal terminal))
         (conversation
           (conversation-create
            configuration :identifier "recovery-vault-capture-during-restore"))
         (application
           (make-instance 'application
                          :configuration configuration
                          :conversation  conversation
                          :ui            ui))
         (vault-pathname
           (configuration-recovery-input-vault-path
            configuration (conversation-pathname conversation)))
         (gate                               (make-lock "Autolith vault restore capture test"))
         (condition
           (make-condition-variable :name "Autolith vault restore capture test"))
         (capture-gate-condition
           (make-condition-variable :name "Autolith vault capture completion test"))
         (restore-read-p                     nil)
         (release-restore-p                  nil)
         (capture-started-p                  nil)
         (capture-completed-p                nil)
         (capture-completed-before-release-p nil)
         (restore-count                      nil)
         (restore-condition                  nil)
         (capture-condition                  nil)
         (controller                         nil)
         (restore-thread                     nil)
         (capture-thread                     nil)
         (original-captures
           (symbol-function 'application-recovery-input-vault-captures)))
    (unwind-protect
         (progn
           (configuration-ensure-directories configuration)
           (conversation-append-user-message conversation "seed")
           (terminal-ui-start ui)
           (setf controller
                 (application-input-controller-create
                  application :load-pending-p nil :start-reader-p nil))
           (ensure-directories-exist vault-pathname)
           (snapshot-write
            vault-pathname
            (recovery-input-vault-tests--vault-form
             conversation
             (list
              (list :id                 "restore-capture"
                    :captured-at        10
                    :active-work        nil
                    :steering-in-flight nil
                    :steering           nil
                    :work               '((:message "restore me")))))
            :mode #o600)
           (setf (symbol-function 'application-recovery-input-vault-captures)
                 (lambda (target-application)
                   (let ((captures (funcall original-captures target-application)))
                     (when (eq target-application application)
                       (with-lock-held (gate)
                         (unless restore-read-p
                           (setf restore-read-p t)
                           (condition-notify condition)
                           (loop until release-restore-p
                                 do (condition-wait condition gate)))))
                     captures))
                 restore-thread
                 (make-thread
                  (lambda ()
                    (handler-case
                        (setf restore-count
                              (application-input-controller-vault-restore controller))
                      (error (error)
                        (setf restore-condition error))))
                  :name "Blocked recovery vault restore"))
           (with-lock-held (gate)
             (loop until restore-read-p
                   unless (condition-wait condition gate :timeout 2)
                     do (error "Timed out waiting for vault restore read.")))
           (setf capture-thread
                 (make-thread
                  (lambda ()
                    (with-lock-held (gate)
                      (setf capture-started-p t)
                     (condition-notify capture-gate-condition))
                    (handler-case
                        (application-recovery-input-vault-capture-message
                         application "concurrent child prompt"
                         :identifier "concurrent-child-capture")
                      (error (error)
                        (setf capture-condition error)))
                    (with-lock-held (gate)
                      (setf capture-completed-p t)
                     (condition-notify capture-gate-condition)))
                  :name "Concurrent recovery vault capture"))
           (with-lock-held (gate)
             (loop until capture-started-p
                   unless (condition-wait capture-gate-condition gate :timeout 2)
                     do (error "Timed out starting concurrent vault capture."))
             (setf capture-completed-before-release-p
                   (loop until capture-completed-p
                         unless (condition-wait capture-gate-condition gate :timeout 1)
                           do (return nil)
                         finally (return t))))
           (test-assert
            (not capture-completed-before-release-p)
            "a concurrent child capture waits behind the restore publication boundary")
           (with-lock-held (gate)
             (setf release-restore-p t)
             (condition-notify condition))
           (join-thread restore-thread)
           (setf restore-thread nil)
           (join-thread capture-thread)
           (setf capture-thread nil)
           (when restore-condition
             (error restore-condition))
           (when capture-condition
             (error capture-condition))
           (let* ((captures (funcall original-captures application))
                  (capture  (first captures)))
             (test-assert
              (and (= restore-count 1)
                   (= (length captures) 1)
                   (string= (getf capture :id) "concurrent-child-capture")
                   (equal (application-recovery-input-vault--capture-work capture)
                          '((:message "concurrent child prompt"))))
              "the child capture written after restore survives vault deletion")))
      (setf (symbol-function 'application-recovery-input-vault-captures)
            original-captures)
      (with-lock-held (gate)
        (setf release-restore-p t)
        (condition-notify condition))
      (when restore-thread
        (ignore-errors (join-thread restore-thread)))
      (when capture-thread
        (ignore-errors (join-thread capture-thread)))
      (when controller
        (ignore-errors (application-input-controller-stop controller)))
      (ignore-errors (terminal-ui-stop ui))
      (platform-delete-directory-tree *platform* root :validate t :if-does-not-exist ':ignore)))
  nil)

(-> test-recovery-input-vault-presentation () null)
(defun test-recovery-input-vault-presentation ()
  "Exercise inspection, storage/read failure, and restore/discard receipt roles."
  (with-test-configuration (configuration)
    (let* ((application    (recovery-input-vault-tests--application configuration "vault-view"))
           (controller     (make-instance 'application-input-controller :application application))
           (captures       (list (list :captured-at (get-universal-time)
                                       :work-items  '((:message "queued 猫")))))
           (failure        (make-condition 'recovery-input-vault-error :message "unreadable fixture"
                                           :pathname                            (test-configuration-root configuration)
                                           :operation                           ':read-vault))
           (read-failure-p nil)
           (entries        nil))
      (setf (application-input-controller application) controller)
      (test-call-with-function-replacements
       (list (list 'application-recovery-input-vault-captures
                   (lambda (application)
                     (declare (ignore application))
                     (if read-failure-p (error failure) captures)))
             (list 'application-input-controller-vault-restore
                   (lambda (controller) (declare (ignore controller)) 1))
             (list 'application-input-controller-vault-discard
                   (lambda (controller) (declare (ignore controller)) t))
             (list 'application-present
                   (lambda (application entry)
                     (declare (ignore application)) (push entry entries) nil)))
       (lambda ()
         (application-operation-call application 'vault)
         (let ((entry (first entries)))
           (test-assert (terminal-styled-text-p entry) "ordinary vault view is structured")
           (test-assert (command-view-tests--role-p entry ':plain "queued 猫") "input previews retain prose styling")
           (test-assert (command-view-tests--role-p entry ':code "(vault-restore)") "restore action uses code styling"))
         (setf (application-recovery-input-vault-failure application) failure
               captures nil)
         (application-operation-call application 'vault)
         (test-assert (command-view-tests--role-p (first entries) ':failure "unreadable fixture")
                      "known storage failures have failure severity")
         (setf read-failure-p t)
         (application-operation-call application 'vault)
         (test-assert (command-view-tests--role-p (first entries) ':failure "unreadable fixture")
                      "read failures retain their severity")
         (test-assert (command-view-tests--role-p (first entries) ':code "(vault-discard)")
                      "read failures expose a structured discard action")
         (application-operation-call application 'vault-restore)
         (test-assert (find ':success (first entries) :key #'terminal-span-style)
                      "successful restoration has success severity")
         (application-operation-call application 'vault-discard)
         (test-assert (find ':success (first entries) :key #'terminal-span-style)
                      "successful discard has success severity")))))
  nil)
