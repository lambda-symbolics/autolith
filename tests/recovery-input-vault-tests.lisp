(in-package #:autolith)

;;;; -- Recovery Input Vault Test Support --

(-> recovery-input-vault-tests--octets (pathname) (simple-array (unsigned-byte 8) (*)))
(defun recovery-input-vault-tests--octets (pathname)
  "Return the exact octets stored at PATHNAME."
  (with-open-file (stream pathname
                          :direction ':input
                          :element-type '(unsigned-byte 8))
    (let ((octets (make-array (file-length stream)
                              :element-type '(unsigned-byte 8))))
      (read-sequence octets stream)
      octets)))

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
                   :conversation conversation)))


;;;; -- Recovery Import --

(-> test-recovery-input-vault-import () null)
(defun test-recovery-input-vault-import ()
  "Test recovery import filters provenance and remains idempotent."
  (let* ((configuration (test-configuration))
         (root (test-configuration-root configuration))
         (application
           (recovery-input-vault-tests--application
            configuration "recovery-vault-import"))
         (conversation (application-conversation application))
         (pending-pathname
           (configuration-pending-inputs-path
            configuration (conversation-pathname conversation)))
         (vault-pathname
           (configuration-recovery-input-vault-path
            configuration (conversation-pathname conversation)))
         (pending-form
           (list :pending-inputs
                 :version 2
                 :snapshot-identifier "snapshot-import"
                 :conversation-id (conversation-identifier conversation)
                 :active-work
                 '(:identifier "active-input"
                   :work (:message "active work"))
                 :steering-in-flight
                 '((:identifier "already-durable" :input "persisted steering")
                   (:identifier "in-flight" :input "in-flight steering"))
                 :steering '("queued steering")
                 :work '((:message "queued follow-up"))
                 :vault-capture-identifiers nil)))
    (unwind-protect
         (progn
           (configuration-ensure-directories configuration)
           (conversation-append-user-message
            conversation "persisted steering"
            :pending-input-identifier "already-durable")
           (ensure-directories-exist pending-pathname)
           (snapshot-write pending-pathname pending-form :mode #o600)
           (test-assert
            (application-recovery-input-vault-import application)
            "recovery import succeeds for one valid pending snapshot")
           (test-assert
            (and (null (application-recovery-input-vault-failure application))
                 (null (application-input-controller application))
                 (not (probe-file pending-pathname)))
            "recovery import neither reports failure nor creates or loads a controller")
           (let* ((captures
                    (application-recovery-input-vault-captures application))
                  (capture (first captures)))
             (test-assert
              (and (= (length captures) 1)
                   (string= (getf capture :id) "snapshot-import")
                   (equal (getf capture :active-work)
                          '(:message "active work"))
                   (equal
                    (mapcar #'agent-steering-input-content
                            (getf capture :steering-in-flight-items))
                    '("in-flight steering"))
                   (equal (getf capture :steering-items)
                          '("queued steering"))
                   (equal (getf capture :work-items)
                          '((:message "queued follow-up"))))
              "recovery import filters durable steering and preserves remaining input")
             ;; Give the existing capture a recognizable timestamp, then replay the
             ;; same pending snapshot as if recovery crashed after vault publication.
             (setf (getf capture :captured-at) 123)
             (application-recovery-input-vault--write-captures
              application captures))
           (snapshot-write pending-pathname pending-form :mode #o600)
           (test-assert
            (application-recovery-input-vault-import application)
            "repeating an imported snapshot succeeds idempotently")
           (let ((captures
                   (application-recovery-input-vault-captures application)))
             (test-assert
              (and (= (length captures) 1)
                   (= (getf (first captures) :captured-at) 123))
              "repeated import keeps the original capture rather than duplicating it"))
           (let ((conflicting-form (copy-tree pending-form))
                 (vault-octets (recovery-input-vault-tests--octets vault-pathname)))
             (setf (getf (getf (rest conflicting-form) :active-work) :work)
                   '(:message "different active work"))
             (snapshot-write pending-pathname conflicting-form :mode #o600)
             (test-assert
              (not (application-recovery-input-vault-import application))
              "a reused snapshot identifier with different input is rejected")
             (test-assert
              (and (typep (application-recovery-input-vault-failure application)
                          'recovery-input-vault-error)
                   (probe-file pending-pathname)
                   (equalp vault-octets
                           (recovery-input-vault-tests--octets vault-pathname)))
              "a conflicting replay preserves both the pending snapshot and vault")))
      (platform-delete-directory-tree *platform* root :validate t :if-does-not-exist ':ignore)))
  nil)

(-> test-recovery-input-vault-legacy-isolation () null)
(defun test-recovery-input-vault-legacy-isolation ()
  "Test legacy migration cleanup never consumes another conversation's input."
  (let* ((configuration (test-configuration))
         (root (test-configuration-root configuration))
         (first-application
           (recovery-input-vault-tests--application
            configuration "recovery-vault-first"))
         (second-application
           (recovery-input-vault-tests--application
            configuration "recovery-vault-second"))
         (second-conversation
           (application-conversation second-application))
         (legacy-pathname
           (configuration-legacy-pending-inputs-path configuration))
         (legacy-form
           (list :pending-inputs
                 :version 1
                 :conversation-id
                 (conversation-identifier second-conversation)
                 :steering '("legacy steering")
                 :work '((:message "legacy work")))))
    (unwind-protect
         (progn
           (configuration-ensure-directories configuration)
           (snapshot-write legacy-pathname legacy-form :mode #o600)
           (test-assert
            (application-recovery-input-vault-import first-application)
            "a valid global legacy snapshot for another conversation is ignored")
           (test-assert
            (and (probe-file legacy-pathname)
                 (null
                  (application-recovery-input-vault-captures first-application)))
            "another conversation's legacy pending input remains untouched")
           (test-assert
            (application-recovery-input-vault-import second-application)
            "the owning conversation imports its global legacy snapshot")
           (let ((captures
                   (application-recovery-input-vault-captures second-application)))
             (test-assert
              (and (= (length captures) 1)
                   (non-empty-string-p (getf (first captures) :id))
                   (null (getf (first captures) :steering-items))
                   (equal (getf (first captures) :work-items)
                          '((:message "legacy steering")
                            (:message "legacy work")))
                   (not (probe-file legacy-pathname)))
              "legacy import generates stable identity and deletes only owned state"))
           ;; Recreate the interrupted migration window: canonical v2 is durable
           ;; while its equivalent legacy source still exists.
           (let* ((third-application
                    (recovery-input-vault-tests--application
                     configuration "recovery-vault-migration"))
                  (third-conversation
                    (application-conversation third-application))
                  (canonical-pathname
                    (configuration-pending-inputs-path
                     configuration (conversation-pathname third-conversation)))
                  (third-legacy-form
                    (list :pending-inputs
                          :version 1
                          :conversation-id
                          (conversation-identifier third-conversation)
                          :steering '("migration steering")
                          :work '((:message "migration work"))))
                  (canonical-form
                    (list :pending-inputs
                          :version 2
                          :snapshot-identifier "migration-snapshot"
                          :conversation-id
                          (conversation-identifier third-conversation)
                          :active-work nil
                          :steering-in-flight nil
                          :steering '("migration steering")
                          :work '((:message "migration work"))
                          :vault-capture-identifiers nil)))
             (snapshot-write legacy-pathname third-legacy-form :mode #o600)
             (ensure-directories-exist canonical-pathname)
             (snapshot-write canonical-pathname canonical-form :mode #o600)
             (test-assert
              (application-recovery-input-vault-import third-application)
              "recovery completes an interrupted legacy-to-canonical migration")
             (test-assert
              (and (not (probe-file legacy-pathname))
                   (not (probe-file canonical-pathname))
                   (string=
                    (getf
                     (first
                      (application-recovery-input-vault-captures
                       third-application))
                     :id)
                    "migration-snapshot"))
              "migration recovery reuses canonical identity and removes both sources")))
      (platform-delete-directory-tree *platform* root :validate t :if-does-not-exist ':ignore)))
  nil)

(-> test-recovery-input-vault-corruption () null)
(defun test-recovery-input-vault-corruption ()
  "Test truncated vault state blocks import without modifying either file."
  (let* ((configuration (test-configuration))
         (root (test-configuration-root configuration))
         (application
           (recovery-input-vault-tests--application
            configuration "recovery-vault-corrupt"))
         (conversation (application-conversation application))
         (pending-pathname
           (configuration-pending-inputs-path
            configuration (conversation-pathname conversation)))
         (vault-pathname
           (configuration-recovery-input-vault-path
            configuration (conversation-pathname conversation)))
         (pending-form
           (list :pending-inputs
                 :version 2
                 :snapshot-identifier "corrupt-vault-pending"
                 :conversation-id (conversation-identifier conversation)
                 :active-work nil
                 :steering-in-flight nil
                 :steering nil
                 :work '((:message "preserve me"))
                 :vault-capture-identifiers nil)))
    (unwind-protect
         (progn
           (configuration-ensure-directories configuration)
           (ensure-directories-exist pending-pathname)
           (snapshot-write pending-pathname pending-form :mode #o600)
           (ensure-directories-exist vault-pathname)
           (with-open-file (stream vault-pathname
                                   :direction ':output
                                   :if-exists ':supersede
                                   :if-does-not-exist ':create
                                   :external-format ':utf-8)
             (write-string "(:recovery-input-vault :version 1" stream))
           (let ((pending-octets
                   (recovery-input-vault-tests--octets pending-pathname))
                 (vault-octets
                   (recovery-input-vault-tests--octets vault-pathname)))
             (test-assert
              (not (application-recovery-input-vault-import application))
              "truncated vault state rejects recovery import")
             (let ((failure
                     (application-recovery-input-vault-failure application)))
               (test-assert
                (and (typep failure 'recovery-input-vault-error)
                     (equal (recovery-input-vault-error-pathname failure)
                            vault-pathname)
                     (equalp pending-octets
                             (recovery-input-vault-tests--octets
                              pending-pathname))
                     (equalp vault-octets
                             (recovery-input-vault-tests--octets
                              vault-pathname)))
                "corrupt vault failure leaves pending and vault bytes unchanged"))
             (test-assert
              (handler-case
                  (progn
                    (application-recovery-input-vault-captures application)
                    nil)
                (recovery-input-vault-error ()
                  t))
              "strict vault inspection reports the same corruption")))
      (platform-delete-directory-tree *platform* root :validate t :if-does-not-exist ':ignore)))
  nil)

(-> test-recovery-input-vault-unattributed-legacy () null)
(defun test-recovery-input-vault-unattributed-legacy ()
  "Test corrupt process-global legacy input remains preserved and unattributed."
  (let* ((configuration (test-configuration))
         (root (test-configuration-root configuration))
         (application
           (recovery-input-vault-tests--application
            configuration "recovery-vault-unattributed-legacy"))
         (conversation (application-conversation application))
         (legacy-pathname
           (configuration-legacy-pending-inputs-path configuration))
         (vault-pathname
           (configuration-recovery-input-vault-path
            configuration (conversation-pathname conversation))))
    (unwind-protect
         (progn
           (configuration-ensure-directories configuration)
           (ensure-directories-exist legacy-pathname)
           (with-open-file (stream legacy-pathname
                                   :direction ':output
                                   :if-exists ':supersede
                                   :if-does-not-exist ':create
                                   :external-format ':utf-8)
             (write-string "(:pending-inputs :version" stream))
           (let ((legacy-octets
                   (recovery-input-vault-tests--octets legacy-pathname)))
             (test-assert
              (application-recovery-input-vault-import application)
              "unattributed corrupt global legacy input does not block recovery")
             (test-assert
              (and (null (application-recovery-input-vault-failure application))
                   (probe-file legacy-pathname)
                   (not (probe-file vault-pathname))
                   (equalp legacy-octets
                           (recovery-input-vault-tests--octets legacy-pathname)))
              "unattributed corrupt global legacy bytes remain untouched")))
      (platform-delete-directory-tree *platform* root :validate t :if-does-not-exist ':ignore)))
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
        :captures captures))

(-> test-recovery-input-vault-restore () null)
(defun test-recovery-input-vault-restore ()
  "Test restore order, image preservation, recalled FIFO, and steering barriers."
  (let* ((configuration (test-configuration))
         (root (test-configuration-root configuration))
         (terminal (make-instance 'waiting-recording-terminal :columns 100))
         (ui (terminal-ui-create :terminal terminal))
         (conversation
           (conversation-create configuration :identifier "recovery-vault-restore"))
         (application
           (make-instance 'application
                          :configuration configuration
                          :conversation conversation
                          :ui ui))
         (image-pathname (merge-pathnames "queued-image.png" root))
         (image-input
           (user-message-input-create
            :text ""
            :image-pathnames (list image-pathname)))
         (vault-pathname
           (configuration-recovery-input-vault-path
            configuration (conversation-pathname conversation)))
         (pending-pathname
           (configuration-pending-inputs-path
            configuration (conversation-pathname conversation)))
         (controller nil))
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
              (list :id "older-capture"
                    :captured-at 10
                    :active-work
                    (list :identifier "active-image"
                          :work
                          (list ':message
                                (application-input--pending-form image-input)))
                    :steering-in-flight
                    '((:identifier "flight" :input "in-flight steering"))
                    :steering '("queued steering")
                    :work '((:message "old follow-up")))
              (list :id "newer-capture"
                    :captured-at 20
                    :active-work nil
                    :steering-in-flight nil
                    :steering nil
                    :work '((:command "/status")))))
            :mode #o600)
           (recording-terminal-reset terminal)
           (application-command application "/vault")
           (let ((output (recording-terminal-output terminal)))
             (test-assert
              (and (search "Recovery input vault" output)
                   (search "old follow-up" output)
                   (search "/status" output))
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
           (application-command application "/vault-restore")
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
                           (application-input-controller--pending-state
                            pending-form (conversation-identifier conversation))))
                    (filtered
                      (and state
                           (application-input-controller--pending-state-filter-persisted
                            state conversation)))
                    (work (and filtered (getf filtered :work-items))))
               (test-assert
                (and complete-p
                     (= (length work) 7)
                     (typep (second (first work)) 'user-message-input)
                     (equal (user-message-input-image-pathnames
                             (second (first work)))
                            (list image-pathname))
                     (string= (second (second work)) "in-flight steering")
                     (string= (second (third work)) "queued steering")
                     (string= (second (fourth work)) "old follow-up")
                     (equal (fifth work) '(:command "/status"))
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
         (root (test-configuration-root configuration))
         (terminal (make-instance 'waiting-recording-terminal :columns 80))
         (ui (terminal-ui-create :terminal terminal))
         (conversation
           (conversation-create
            configuration :identifier "recovery-vault-active-restore-crash"))
         (application
           (make-instance 'application
                          :configuration configuration
                          :conversation conversation
                          :ui ui))
         (vault-pathname
           (configuration-recovery-input-vault-path
            configuration (conversation-pathname conversation)))
         (controller nil))
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
              (list :id "active-restore-capture"
                    :captured-at 10
                    :active-work nil
                    :steering-in-flight nil
                    :steering nil
                    :work '((:message "restored active")
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
           (test-assert
            (application-recovery-input-vault-import application)
            "a crash snapshot during restored active work remains vaultable")
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
  (let* ((configuration (test-configuration))
         (root (test-configuration-root configuration))
         (terminal (make-instance 'waiting-recording-terminal :columns 80))
         (ui (terminal-ui-create :terminal terminal))
         (conversation
           (conversation-create configuration :identifier "recovery-vault-rollback"))
         (application
           (make-instance 'application
                          :configuration configuration
                          :conversation conversation
                          :ui ui))
         (vault-pathname
           (configuration-recovery-input-vault-path
            configuration (conversation-pathname conversation)))
         (pending-pathname
           (configuration-pending-inputs-path
            configuration (conversation-pathname conversation)))
         (controller nil)
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
              (list :id "rollback-capture"
                    :captured-at 10
                    :active-work nil
                    :steering-in-flight nil
                    :steering nil
                    :work '((:message "restored work")))))
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
  (let* ((configuration (test-configuration))
         (root (test-configuration-root configuration))
         (terminal (make-instance 'waiting-recording-terminal :columns 80))
         (ui (terminal-ui-create :terminal terminal))
         (conversation
           (conversation-create
            configuration :identifier "recovery-vault-post-delete-rollback"))
         (application
           (make-instance 'application
                          :configuration configuration
                          :conversation conversation
                          :ui ui))
         (vault-pathname
           (configuration-recovery-input-vault-path
            configuration (conversation-pathname conversation)))
         (pending-pathname
           (configuration-pending-inputs-path
            configuration (conversation-pathname conversation)))
         (controller nil)
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
              (list :id "post-delete-capture"
                    :captured-at 10
                    :active-work nil
                    :steering-in-flight nil
                    :steering nil
                    :work '((:message "restored work")))))
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
              "failed final publication retains restored work with provenance"))
           (setf
            (symbol-function
             'application-input-controller--publish-pending-publication-locked)
            original-publisher)
           (test-assert
            (application-recovery-input-vault-import application)
            "the provenance-bearing snapshot remains importable")
           (let ((captures
                   (application-recovery-input-vault-captures application)))
             (test-assert
              (and (= (length captures) 1)
                   (equal (getf (first captures) :work-items)
                          '((:message "restored work")
                            (:message "existing work")))
                   (not (probe-file pending-pathname)))
              "recovery reconstructs exactly one capture without loss or duplication")))
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
         (root (test-configuration-root configuration))
         (terminal (make-instance 'waiting-recording-terminal :columns 80))
         (ui (terminal-ui-create :terminal terminal))
         (conversation
           (conversation-create configuration :identifier "recovery-vault-discard"))
         (other-conversation
           (conversation-create configuration :identifier "recovery-vault-other"))
         (application
           (make-instance 'application
                          :configuration configuration
                          :conversation conversation
                          :ui ui))
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
         (controller nil))
    (unwind-protect
         (progn
           (configuration-ensure-directories configuration)
           (conversation-append-user-message conversation "seed")
           (conversation-append-user-message other-conversation "seed")
           (ensure-directories-exist current-vault)
           (with-open-file (stream current-vault
                                   :direction ':output
                                   :if-exists ':supersede
                                   :if-does-not-exist ':create
                                   :external-format ':utf-8)
             (write-string "(:corrupt" stream))
           (ensure-directories-exist other-vault)
           (snapshot-write
            other-vault
            (recovery-input-vault-tests--vault-form
             other-conversation
             (list
              (list :id "other-capture"
                    :captured-at 10
                    :active-work nil
                    :steering-in-flight nil
                    :steering nil
                    :work '((:message "other work")))))
            :mode #o600)
           (ensure-directories-exist pending-pathname)
           (snapshot-write
            pending-pathname
            (list :pending-inputs
                  :version 2
                  :snapshot-identifier "discard-pending"
                  :conversation-id (conversation-identifier conversation)
                  :active-work nil
                  :steering-in-flight nil
                  :steering nil
                  :work '((:message "discard me"))
                  :vault-capture-identifiers nil
                  :steering-promotion-prefix-count 0)
            :mode #o600)
           (ensure-directories-exist legacy-pathname)
           (with-open-file (stream legacy-pathname
                                   :direction ':output
                                   :if-exists ':supersede
                                   :if-does-not-exist ':create
                                   :external-format ':utf-8)
             (write-string "(:pending-inputs :version" stream))
           (setf legacy-octets
                 (recovery-input-vault-tests--octets legacy-pathname))
           (terminal-ui-start ui)
           (setf controller
                 (application-input-controller-create
                  application
                  :load-pending-p nil
                  :pending-persistence-enabled-p nil)
                 (application-recovery-input-vault-failure application)
                 (make-condition
                  'recovery-input-vault-error
                  :message "corrupt recovery state"
                  :pathname current-vault
                  :operation ':read-vault))
           (recording-terminal-reset terminal)
           (application-command application "/vault-discard")
           (test-assert
            (and (search "Discarded" (recording-terminal-output terminal))
                 (not (probe-file current-vault))
                 (not (probe-file pending-pathname))
                 (probe-file other-vault)
                 (probe-file legacy-pathname)
                 (equalp legacy-octets
                         (recovery-input-vault-tests--octets legacy-pathname))
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
         (root (test-configuration-root configuration))
         (terminal (make-instance 'waiting-recording-terminal :columns 80))
         (ui (terminal-ui-create :terminal terminal))
         (conversation
           (conversation-create configuration :identifier "recovery-vault-ingress"))
         (application
           (make-instance 'application
                          :configuration configuration
                          :conversation conversation
                          :ui ui))
         (controller nil))
    (unwind-protect
         (progn
           (configuration-ensure-directories configuration)
           (conversation-append-user-message conversation "seed")
           (terminal-ui-start ui)
           (setf controller
                 (application-input-controller-create
                  application
                  :load-pending-p nil
                  :pending-persistence-enabled-p nil))
           (recording-terminal-reset terminal)
           (application-input-controller--handle-submission
            controller "ordinary message")
           (application-input-controller--handle-submission controller "/status")
           (test-assert
            (and (null (application-input-controller--state controller :work-items))
                 (null (application-input-controller--state
                        controller :steering-items))
                 (search "Recovered input storage is unavailable"
                         (recording-terminal-output terminal)))
            "disabled recovery storage rejects ordinary messages and commands")
           (test-assert
            (every
             (lambda (command)
               (application-input-controller--submission-storage-ready-p
                controller command))
             '("/vault" "/vault-restore" "/vault-discard"))
            "disabled recovery storage admits every exact vault control")
           (application-input-controller--handle-submission controller "/vault")
           (test-assert
            (equal (application-input-controller--state controller :work-items)
                   '((:command "/vault")))
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
         (root (test-configuration-root configuration))
         (terminal (make-instance 'waiting-recording-terminal :columns 80))
         (ui (terminal-ui-create :terminal terminal))
         (conversation
           (conversation-create
            configuration :identifier "recovery-vault-recalled-ingress"))
         (application
           (make-instance 'application
                          :configuration configuration
                          :conversation conversation
                          :ui ui))
         (pending-pathname
           (configuration-pending-inputs-path
            configuration (conversation-pathname conversation)))
         (controller nil))
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
                 (search "Recovered input storage is unavailable"
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
    (configuration string terminal &key (:recovery-startup-p boolean))
    application)
(defun recovery-input-vault-tests--startup-application
    (configuration identifier terminal &key (recovery-startup-p nil))
  "Return a complete minimal startup application for one test conversation."
  (let ((conversation
          (conversation-create configuration :identifier identifier)))
    (conversation-append-user-message conversation "seed")
    (make-instance 'application
                   :configuration configuration
                   :conversation conversation
                   :provider nil
                   :tool-registry (make-instance 'tool-registry)
                   :worker nil
                   :agent nil
                   :ui (terminal-ui-create :terminal terminal)
                   :recovery-startup-p recovery-startup-p)))

(-> test-recovery-input-vault-recovery-startup () null)
(defun test-recovery-input-vault-recovery-startup ()
  "Test recovery startup vaults pending input before controller creation."
  (let* ((configuration (test-configuration))
         (root (test-configuration-root configuration))
         (terminal (make-instance 'waiting-recording-terminal :columns 90))
         (application
           (recovery-input-vault-tests--startup-application
            configuration
            "recovery-vault-startup"
            terminal
            :recovery-startup-p t))
         (conversation (application-conversation application))
         (pending-pathname
           (configuration-pending-inputs-path
            configuration (conversation-pathname conversation)))
         (vault-pathname
           (configuration-recovery-input-vault-path
            configuration (conversation-pathname conversation)))
         (original-create
           (symbol-function 'application-input-controller-create))
         (observed-load-pending-p :unset)
         (observed-persistence-p :unset)
         (observed-start-reader-p :unset)
         (pending-at-create-p :unset)
         (vault-at-create-p :unset)
         (observed-first-work :unset)
         (observed-work :unset)
         (observed-steering :unset))
    (unwind-protect
         (progn
           (configuration-ensure-directories configuration)
           (ensure-directories-exist pending-pathname)
           (snapshot-write
            pending-pathname
            (list :pending-inputs
                  :version 2
                  :snapshot-identifier "startup-pending"
                  :conversation-id (conversation-identifier conversation)
                  :active-work nil
                  :steering-in-flight nil
                  :steering '("recovered steering")
                  :work '((:message "recovered follow-up"))
                  :vault-capture-identifiers nil
                  :steering-promotion-prefix-count 0)
            :mode #o600)
           (test-call-with-function-replacements
            (list
             (list
              'application-input-controller-create
              (lambda (run-application
                       &key initial-work-items (load-pending-p t)
                            (pending-persistence-enabled-p t)
                            (start-reader-p t))
                (setf observed-load-pending-p load-pending-p
                      observed-persistence-p pending-persistence-enabled-p
                      observed-start-reader-p start-reader-p
                      pending-at-create-p (not (null (probe-file pending-pathname)))
                      vault-at-create-p (not (null (probe-file vault-pathname))))
                (funcall original-create
                         run-application
                         :initial-work-items initial-work-items
                         :load-pending-p load-pending-p
                         :pending-persistence-enabled-p
                         pending-persistence-enabled-p
                         :start-reader-p start-reader-p)))
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
                nil))
             (list 'localgroup-start
                   (lambda (run-application)
                     (declare (ignore run-application))
                     nil))
             (list 'localgroup-stop
                   (lambda (run-application)
                     (declare (ignore run-application))
                     nil)))
            (lambda ()
              (application-run
               application :recovery-diagnosis "diagnose recovered crash")))
           (let ((output (recording-terminal-output terminal))
                 (captures
                   (application-recovery-input-vault-captures application)))
             (test-assert
              (and (null observed-load-pending-p)
                   observed-persistence-p
                   (null observed-start-reader-p)
                   (null pending-at-create-p)
                   vault-at-create-p
                   (not (probe-file pending-pathname))
                   (probe-file vault-pathname))
              "recovery vaulting precedes nonloading controller creation without a reader")
              (test-assert
               (not (application-recovery-startup-p application))
               "recovery startup consumes its one-shot transcript replay state")
             (test-assert
              (and (equal observed-first-work
                          '(:recovery-diagnosis "diagnose recovered crash"))
                   (null observed-work)
                   (null observed-steering))
              "the real scheduler selects recovery diagnosis before any user input")
             (test-assert
              (and (= (length captures) 1)
                   (null (getf (first captures) :steering-items))
                   (equal (getf (first captures) :work-items)
                          '((:message "recovered steering")
                            (:message "recovered follow-up"))))
              "startup places recovered pending input only in the vault")
             (test-assert
              (and (search "Nothing was submitted automatically." output)
                   (search "/vault" output)
                   (search "/vault-restore" output)
                   (search "/vault-discard" output))
              "recovery startup warns how to inspect, restore, or discard vaulted input")))
      (platform-delete-directory-tree *platform* root :validate t :if-does-not-exist ':ignore)))
  nil)

(-> test-recovery-input-vault-corrupt-startup () null)
(defun test-recovery-input-vault-corrupt-startup ()
  "Test corrupt recovery storage blocks ingress without loading pending input."
  (let* ((configuration (test-configuration))
         (root (test-configuration-root configuration))
         (terminal (make-instance 'waiting-recording-terminal :columns 90))
         (application
           (recovery-input-vault-tests--startup-application
            configuration
            "recovery-vault-corrupt-startup"
            terminal
            :recovery-startup-p t))
         (conversation (application-conversation application))
         (pending-pathname
           (configuration-pending-inputs-path
            configuration (conversation-pathname conversation)))
         (vault-pathname
           (configuration-recovery-input-vault-path
            configuration (conversation-pathname conversation)))
         (original-create
           (symbol-function 'application-input-controller-create))
         (pending-octets nil)
         (vault-octets nil)
         (observed-load-pending-p :unset)
         (observed-persistence-p :unset)
         (observed-start-reader-p :unset)
         (observed-first-work :unset)
         (observed-work :unset)
         (observed-steering :unset)
         (ingress-blocked-p nil)
         (vault-inspect-executed-p nil)
         (observed-failure nil)
         (bytes-preserved-p nil)
         (vault-controls-admitted-p nil))
    (unwind-protect
         (progn
           (configuration-ensure-directories configuration)
           (ensure-directories-exist pending-pathname)
           (snapshot-write
            pending-pathname
            (list :pending-inputs
                  :version 2
                  :snapshot-identifier "corrupt-startup-pending"
                  :conversation-id (conversation-identifier conversation)
                  :active-work nil
                  :steering-in-flight nil
                  :steering nil
                  :work '((:message "preserve pending bytes"))
                  :vault-capture-identifiers nil
                  :steering-promotion-prefix-count 0)
            :mode #o600)
           (ensure-directories-exist vault-pathname)
           (with-open-file (stream vault-pathname
                                   :direction ':output
                                   :if-exists ':supersede
                                   :if-does-not-exist ':create
                                   :external-format ':utf-8)
             (write-string "(:recovery-input-vault :version 1" stream))
           (setf pending-octets
                 (recovery-input-vault-tests--octets pending-pathname)
                 vault-octets
                 (recovery-input-vault-tests--octets vault-pathname))
           (test-call-with-function-replacements
            (list
             (list
              'application-input-controller-create
              (lambda (run-application
                       &key initial-work-items (load-pending-p t)
                            (pending-persistence-enabled-p t)
                            (start-reader-p t))
                (setf observed-load-pending-p load-pending-p
                      observed-persistence-p pending-persistence-enabled-p
                      observed-start-reader-p start-reader-p)
                (funcall original-create
                         run-application
                         :initial-work-items initial-work-items
                         :load-pending-p load-pending-p
                         :pending-persistence-enabled-p
                         pending-persistence-enabled-p
                         :start-reader-p start-reader-p)))
             (list
              'application-input-controller--run-work
              (lambda (controller work)
                (setf observed-first-work (copy-tree work)
                      observed-work
                      (application-input-controller--state controller :work-items)
                      observed-steering
                      (application-input-controller--state controller :steering-items))
                (labels ((submit (input)
                           (terminal-ui-set-input
                            (application-ui application) input)
                           (application-input-controller--process-event
                            controller ':submit)))
                  (submit "blocked after recovery failure")
                  (submit "/status")
                  (setf ingress-blocked-p
                        (and
                         (null (application-input-controller--state
                                controller :work-items))
                         (null (application-input-controller--state
                                controller :steering-items))))
                  (submit "/vault")
                  (setf vault-inspect-executed-p
                        (not
                         (null
                          (search "Recovery input vault could not be read"
                                  (recording-terminal-output terminal)))))
                  (setf observed-failure
                        (application-recovery-input-vault-failure application))
                  ;; Startup itself must not have touched the corrupt bytes
                  ;; before any vault control runs.
                  (setf bytes-preserved-p
                        (and (probe-file pending-pathname)
                             (probe-file vault-pathname)
                             (equalp pending-octets
                                     (recovery-input-vault-tests--octets
                                      pending-pathname))
                             (equalp vault-octets
                                     (recovery-input-vault-tests--octets
                                      vault-pathname))
                             t))
                  (submit "/vault-restore")
                  (submit "/vault-discard")
                  ;; Vault controls manage the blocked queue itself, so they
                  ;; execute immediately instead of joining that queue; the
                  ;; executed discard resolves the corrupt vault state.
                  (setf vault-controls-admitted-p
                        (and (null (application-input-controller--state
                                    controller :work-items))
                             (search "Discarded this conversation's recovered input."
                                     (recording-terminal-output terminal))
                             (null (application-recovery-input-vault-failure
                                    application))
                             t)))
                (with-lock-held ((application-input-controller-lock controller))
                  (setf (application-input-controller-stopping-p controller) t)
                  (sb-thread:condition-broadcast
                   (application-input-controller-condition-variable controller)))
                nil))
             (list 'localgroup-start
                   (lambda (run-application)
                     (declare (ignore run-application))
                     nil))
             (list 'localgroup-stop
                   (lambda (run-application)
                     (declare (ignore run-application))
                     nil)))
            (lambda ()
              (application-run
               application :recovery-diagnosis "diagnose corrupt recovery")))
           (let ((output (recording-terminal-output terminal)))
             (test-assert
              (and (null observed-load-pending-p)
                   (null observed-persistence-p)
                   (null observed-start-reader-p)
                   (typep observed-failure 'recovery-input-vault-error)
                   ingress-blocked-p
                   vault-inspect-executed-p
                   vault-controls-admitted-p)
              "corrupt recovery blocks ordinary terminal submissions and admits vault controls")
             (test-assert
              (and (equal observed-first-work
                          '(:recovery-diagnosis "diagnose corrupt recovery"))
                   (null observed-work)
                   (null observed-steering))
              "corrupt pending input remains outside the real startup scheduler")
             (test-assert
              bytes-preserved-p
              "corrupt startup preserves exact pending and vault bytes")
             (test-assert
              (and (not (probe-file vault-pathname))
                   (not (probe-file pending-pathname)))
              "the executed vault discard removes the corrupt storage")
             (test-assert
              (and (search "New submissions are blocked" output)
                   (search "Nothing was submitted automatically." output)
                   (search "/vault" output)
                   (search "/vault-restore" output)
                   (search "/vault-discard" output))
              "corrupt startup warns without submitting or hiding vault controls")))
      (platform-delete-directory-tree *platform* root :validate t :if-does-not-exist ':ignore)))
  nil)

(-> test-recovery-input-vault-ordinary-startup () null)
(defun test-recovery-input-vault-ordinary-startup ()
  "Test ordinary startup retains normal pending-input loading."
  (let* ((configuration (test-configuration))
         (root (test-configuration-root configuration))
         (terminal (make-instance 'waiting-recording-terminal :columns 90))
         (application
           (recovery-input-vault-tests--startup-application
            configuration "recovery-vault-ordinary-startup" terminal))
         (conversation (application-conversation application))
         (pending-pathname
           (configuration-pending-inputs-path
            configuration (conversation-pathname conversation)))
         (vault-pathname
           (configuration-recovery-input-vault-path
            configuration (conversation-pathname conversation)))
         (original-create
           (symbol-function 'application-input-controller-create))
         (observed-load-pending-p :unset)
         (observed-persistence-p :unset)
         (observed-start-reader-p :unset)
         (observed-first-work :unset)
         (observed-work :unset)
         (observed-steering :unset))
    (unwind-protect
         (progn
           (configuration-ensure-directories configuration)
           (ensure-directories-exist pending-pathname)
           (snapshot-write
            pending-pathname
            (list :pending-inputs
                  :version 2
                  :snapshot-identifier "ordinary-startup-pending"
                  :conversation-id (conversation-identifier conversation)
                  :active-work nil
                  :steering-in-flight nil
                  :steering '("ordinary steering")
                  :work '((:message "ordinary follow-up"))
                  :vault-capture-identifiers nil
                  :steering-promotion-prefix-count 0)
            :mode #o600)
           (test-call-with-function-replacements
            (list
             (list
              'application-input-controller-create
              (lambda (run-application
                       &key initial-work-items (load-pending-p t)
                            (pending-persistence-enabled-p t)
                            (start-reader-p t))
                (setf observed-load-pending-p load-pending-p
                      observed-persistence-p pending-persistence-enabled-p
                      observed-start-reader-p start-reader-p)
                (funcall original-create
                         run-application
                         :initial-work-items initial-work-items
                         :load-pending-p load-pending-p
                         :pending-persistence-enabled-p
                         pending-persistence-enabled-p
                         :start-reader-p start-reader-p)))
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
                nil))
             (list 'localgroup-start
                   (lambda (run-application)
                     (declare (ignore run-application))
                     nil))
             (list 'localgroup-stop
                   (lambda (run-application)
                     (declare (ignore run-application))
                     nil)))
            (lambda ()
              (application-run
               application
               :initial-input (user-message-input-create :text "draft"))))
           (let ((output (recording-terminal-output terminal)))
             (test-assert
              (and observed-load-pending-p
                   observed-persistence-p
                   (null observed-start-reader-p)
                   (equal observed-first-work
                          '(:message "ordinary steering"))
                   (equal observed-work
                          '((:message "ordinary follow-up")))
                   (null observed-steering))
              "ordinary startup still schedules conversation-scoped pending input")
             (test-assert
              (and (probe-file pending-pathname)
                   (not (probe-file vault-pathname))
                   (not (search "Nothing was submitted automatically." output)))
              "ordinary startup neither vaults pending input nor presents a recovery warning")))
      (platform-delete-directory-tree *platform* root :validate t :if-does-not-exist ':ignore)))
  nil)

(-> test-recovery-input-vault-live-primary-submit () null)
(defun test-recovery-input-vault-live-primary-submit ()
  "Test accepted primary follow-ups are vaulted immediately and removed when consumed."
  (let* ((configuration (test-configuration))
         (root (test-configuration-root configuration))
         (terminal (make-instance 'waiting-recording-terminal :columns 80))
         (ui (terminal-ui-create :terminal terminal))
         (conversation
           (conversation-create configuration :identifier "recovery-vault-live-submit"))
         (application
           (make-instance 'application
                          :configuration configuration
                          :conversation conversation
                          :ui ui))
         (pending-pathname
           (configuration-pending-inputs-path
            configuration (conversation-pathname conversation)))
         (controller nil))
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
         (root (test-configuration-root configuration))
         (application
           (recovery-input-vault-tests--application
            configuration "recovery-vault-capture-message"))
         (conversation (application-conversation application))
         (identifier nil))
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
                                   :conversation conversation))
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
  (let* ((configuration (test-configuration))
         (root (test-configuration-root configuration))
         (terminal (make-instance 'waiting-recording-terminal :columns 80))
         (ui (terminal-ui-create :terminal terminal))
         (conversation
           (conversation-create
            configuration :identifier "recovery-vault-capture-during-restore"))
         (application
           (make-instance 'application
                          :configuration configuration
                          :conversation conversation
                          :ui ui))
         (vault-pathname
           (configuration-recovery-input-vault-path
            configuration (conversation-pathname conversation)))
         (gate (make-lock "Autolith vault restore capture test"))
         (condition
           (make-condition-variable :name "Autolith vault restore capture test"))
         (capture-gate-condition
           (make-condition-variable :name "Autolith vault capture completion test"))
         (restore-read-p nil)
         (release-restore-p nil)
         (capture-started-p nil)
         (capture-completed-p nil)
         (capture-completed-before-release-p nil)
         (restore-count nil)
         (restore-condition nil)
         (capture-condition nil)
         (controller nil)
         (restore-thread nil)
         (capture-thread nil)
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
              (list :id "restore-capture"
                    :captured-at 10
                    :active-work nil
                    :steering-in-flight nil
                    :steering nil
                    :work '((:message "restore me")))))
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
                  (capture (first captures)))
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

(-> run-recovery-input-vault-tests () boolean)
(defun run-recovery-input-vault-tests ()
  "Run focused recovery input vault tests and return true on success."
  (test-recovery-input-vault-import)
  (test-recovery-input-vault-legacy-isolation)
  (test-recovery-input-vault-corruption)
  (test-recovery-input-vault-unattributed-legacy)
  (test-recovery-input-vault-restore)
  (test-recovery-input-vault-active-restore-crash)
  (test-recovery-input-vault-restore-rollback)
  (test-recovery-input-vault-post-delete-rollback)
  (test-recovery-input-vault-discard)
  (test-recovery-input-vault-disabled-ingress)
  (test-recovery-input-vault-disabled-recalled-ingress)
  (test-recovery-input-vault-recovery-startup)
  (test-recovery-input-vault-corrupt-startup)
  (test-recovery-input-vault-ordinary-startup)
  (test-recovery-input-vault-live-primary-submit)
  (test-recovery-input-vault-capture-message)
  (test-recovery-input-vault-capture-during-restore)
  t)
