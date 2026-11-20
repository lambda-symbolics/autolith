(in-package #:autolith)

;;;; -- Project Adaptation Tests --

(defvar *project-adaptation-test-read-eval-p* nil
  "Whether a hostile offer-state form was evaluated during a test.")

(-> project-adaptation-tests--append-late-turn
    (conversation integer)
    null)
(defun project-adaptation-tests--append-late-turn (conversation seconds-after)
  "Append one synthetic user turn SECONDS-AFTER conversation creation."
  (let ((record
          (list :message
                :seq (conversation-next-sequence conversation)
                :time    (+ (conversation-created-at conversation) seconds-after)
                :role    ':user
                :content "later project work")))
    (log-append (conversation-log-pathname conversation) record)
    (conversation--note-activity conversation record)
    (incf (conversation-next-sequence conversation)))
  nil)

(-> project-adaptation-tests--substantial-conversation
    (configuration string)
    conversation)
(defun project-adaptation-tests--substantial-conversation
    (configuration identifier)
  "Create and persist one hour-long synthetic conversation."
  (let ((conversation
          (conversation-create configuration :identifier identifier)))
    (conversation-append-user-message conversation "begin project work")
    (project-adaptation-tests--append-late-turn
     conversation
     *project-adaptation-substantial-seconds*)
    conversation))

(-> project-adaptation-tests--run-offer (configuration conversation list) string)
(defun project-adaptation-tests--run-offer
    (configuration conversation events)
  "Run one scripted project-adaptation offer and return terminal output."
  (let* ((terminal
           (make-instance 'scripted-terminal :columns 80 :events events))
         (application
           (make-instance 'application
                          :configuration configuration
                          :conversation  conversation
                          :ui            (terminal-ui-create :terminal terminal))))
    (with-terminal-ui (active-ui (application-ui application))
      (application-maybe-offer-project-adaptation application))
    (recording-terminal-output terminal)))

(-> project-adaptation-tests--reverse-metadata (configuration pathname) null)
(defun project-adaptation-tests--reverse-metadata (configuration project-root)
  "Observe actual segment reads for matching and foreign project histories."
  (let* ((conversation (conversation-create configuration :identifier "reverse-qualify"))
         (identity     (conversation-pathname conversation))
         (project-key  (project-adaptation--project-key project-root))
         (options      (symbol-function 'conversation--segment-options))
         (visited      nil))
    (flet ((scan (key)
             "Observe the actual segment headers read by the storage mapper."
             (setf visited nil)
             (test-call-with-function-replacements
              (list (list 'conversation--segment-options
                          (lambda (pathname &key on-header)
                            (funcall options pathname :on-header
                                     (lambda (segment)
                                       (push (conversation-log-pathname segment) visited)
                                       (when on-header (funcall on-header segment)))))))
              (lambda () (project-adaptation--conversation-metadata identity key)))))
      (conversation-append-user-message conversation "older")
      (conversation-append-summary conversation (make-continuation :intent "First checkpoint"))
      (project-adaptation-tests--append-late-turn conversation *project-adaptation-substantial-seconds*)
      (conversation-append-summary conversation (make-continuation :intent "Second checkpoint"))
      (project-adaptation-tests--append-late-turn conversation 1)
      (let ((metadata (scan project-key)))
        (test-assert (project-adaptation--substantial-conversation-p metadata)
                     "a later sequence with a short timestamp does not hide a qualifying record")
        (test-assert (equal (reverse visited)
                            (subseq (reverse (conversation-storage-pathnames identity)) 0 2))
                     "read the newest two real chunks and stop before the oldest"))
      (test-assert (and (null (scan "foreign project")) (null visited))
                   "a foreign header requires no record reads")))
  nil)

(-> test-project-adaptations () null)
(defun test-project-adaptations ()
  "Test project notes, offer decisions, qualification, and ephemeral advice."
  (let* ((base           (test-configuration))
         (temporary-root (test-configuration-root base))
         (repository     (merge-pathnames "project/" temporary-root))
         (nested         (merge-pathnames "src/deep/" repository)))
    (unwind-protect
         (progn
           (ensure-directories-exist (merge-pathnames ".git/" repository))
           (ensure-directories-exist nested)
           (let* ((configuration
                    (configuration-copy base :working-directory nested))
                  (project-root (workspace-project-root nested))
                  (notes        (workspace-autolith-notes-path nested))
                  (now          4000000000))
             (configuration-ensure-directories configuration)
             (project-adaptation-tests--reverse-metadata configuration project-root)
             (conversation-delete configuration "reverse-qualify")
             (test-assert (equal project-root (truename repository))
                          "project identity walks from a nested workspace to Git")
             (test-assert (equal notes (merge-pathnames "AUTOLITH.org" repository))
                          "adaptation notes live only at the project root")
             (test-assert
              (project-adaptation-offer-due-p configuration project-root now)
              "a new project is initially eligible for an adaptation offer")
             (project-adaptation-offer-defer configuration project-root now)
             (test-assert
              (not
               (project-adaptation-offer-due-p
                configuration
                project-root
                (1- (+ now *project-adaptation-offer-deferral-seconds*))))
              "not-now suppresses offers throughout five complete days")
             (test-assert
              (project-adaptation-offer-due-p
               configuration
               project-root
               (+ now *project-adaptation-offer-deferral-seconds*))
              "the offer becomes eligible at the exact five-day boundary")
             (project-adaptation-offer-refuse configuration project-root)
             (test-assert
              (not
               (project-adaptation-offer-due-p
                configuration project-root most-positive-fixnum))
              "never permanently suppresses offers for the canonical path")
             (let* ((path (project-adaptation--project-key project-root))
                    (duplicate
                      (list :path path :deferred-until now :never-p nil)))
               (snapshot-write
                (configuration-project-adaptation-offers-path configuration)
                (list ':project-adaptation-offers
                      ':version *project-adaptation-offer-state-version*
                      ':entries (list duplicate duplicate)))
               (test-assert
                (handler-case
                    (progn
                      (project-adaptation--offer-state-read configuration)
                      nil)
                  (project-adaptation-error ()
                    t))
                "duplicate project keys are rejected when loading state"))
             (delete-file
              (configuration-project-adaptation-offers-path configuration))
             (let ((state-pathname
                     (configuration-project-adaptation-offers-path
                      configuration)))
               (setf *project-adaptation-test-read-eval-p* nil)
               (with-open-file (stream state-pathname
                                       :direction         ':output
                                       :if-exists         ':supersede
                                       :if-does-not-exist ':create)
                 (write-string
                  "#.(setf autolith::*project-adaptation-test-read-eval-p* t)"
                  stream))
               (test-assert
                (handler-case
                    (progn
                      (project-adaptation--offer-state-read configuration)
                      nil)
                  (project-adaptation-error ()
                    t))
                "read-time evaluation is rejected as malformed offer state")
               (test-assert
                (not *project-adaptation-test-read-eval-p*)
                "reading offer state keeps read-time evaluation disabled")
               (delete-file state-pathname))
             (let* ((other-project
                      (merge-pathnames "concurrent-project/" temporary-root))
                    (threads
                      (progn
                        (ensure-directories-exist other-project)
                        (list
                         (make-thread
                          (lambda ()
                            (project-adaptation-offer-defer
                             configuration project-root now)))
                         (make-thread
                          (lambda ()
                            (project-adaptation-offer-refuse
                             configuration other-project)))))))
               (dolist (thread threads)
                 (join-thread thread))
               (test-assert
                (= (length
                    (project-adaptation--offer-state-read configuration))
                   2)
                "concurrent project decisions preserve both state entries"))
             (delete-file
              (configuration-project-adaptation-offers-path configuration))
             (let ((short
                     (conversation-create configuration :identifier "short")))
               (conversation-append-user-message short "small request")
               (test-assert
                (not
                 (project-adaptation-resume-qualifies-p configuration short))
                "one short conversation does not trigger the resume offer")
               (project-adaptation-tests--substantial-conversation
                configuration "substantial-one")
               (test-assert
                (not
                 (project-adaptation-resume-qualifies-p configuration short))
                "one other substantial conversation remains insufficient")
               (project-adaptation-tests--substantial-conversation
                configuration "substantial-two")
               (test-assert
                (project-adaptation-resume-qualifies-p configuration short)
                "two substantial project conversations qualify a short resume"))
             (let* ((malformed
                      (conversation-create configuration
                                           :identifier "malformed-tail"))
                    (pathname (conversation-pathname malformed)))
               (conversation-append-user-message malformed "begin")
               (project-adaptation-tests--append-late-turn
                malformed *project-adaptation-substantial-seconds*)
               (with-open-file (stream (conversation-log-pathname malformed)
                                       :direction       ':output
                                       :if-exists       ':append
                                       :external-format ':utf-8)
                 (write-char #\( stream))
               (test-assert
                (project-adaptation--substantial-conversation-p
                  (project-adaptation--conversation-metadata pathname (project-adaptation--project-key project-root)))
                "resume qualification tolerates an incomplete history tail"))
             (let ((touched
                     (conversation-create configuration
                                          :identifier "touched-short")))
               (conversation-append-user-message touched "still short")
               (platform-set-file-times
                *platform*
                (conversation-log-pathname touched)
                (+ (conversation-created-at touched)
                   *project-adaptation-substantial-seconds*))
               (test-assert
                (not
                 (project-adaptation--substantial-conversation-p
                  (project-adaptation--conversation-metadata
                    (conversation-pathname touched) (project-adaptation--project-key project-root))))
                "filesystem timestamps do not masquerade as recorded activity"))
             (let ((legacy-pathname
                     (merge-pathnames
                      "legacy-incomplete.sexp"
                      (configuration-conversation-root configuration))))
               (with-open-file (stream legacy-pathname
                                       :direction         ':output
                                       :if-exists         ':supersede
                                       :if-does-not-exist ':create
                                       :external-format   ':utf-8)
                 (write (list :conversation
                              :version 1
                              :id         "legacy-incomplete"
                              :created-at nil
                              :directory  (namestring nested))
                        :stream   stream
                        :readably t)
                 (terpri stream)
                 (dotimes (index *project-adaptation-fallback-user-turns*)
                   (write (list :message
                                :seq (1+ index)
                                :role    ':user
                                :content "legacy turn")
                          :stream   stream
                          :readably t)
                   (terpri stream))
                 (write-char #\( stream))
               (test-assert
                (project-adaptation--substantial-conversation-p
                  (project-adaptation--conversation-metadata legacy-pathname (project-adaptation--project-key project-root)))
                "legacy fallback tolerates an incomplete final form"))
             (let* ((other-repository
                      (merge-pathnames "other-project/" temporary-root))
                    (other-configuration nil)
                    (mismatched
                      (conversation-create configuration
                                           :identifier "mismatched")))
               (ensure-directories-exist
                (merge-pathnames ".git/" other-repository))
               (setf other-configuration
                     (configuration-copy
                      configuration :working-directory other-repository))
               (test-assert
                (not
                 (project-adaptation-resume-qualifies-p
                  other-configuration mismatched))
                "history from another project does not qualify the current path"))
             (test-assert
              (project-adaptation--substantial-conversation-p
               (list :created-at       100
                     :last-activity-at 3700
                     :user-turns       1))
              "a recorded one-hour activity span is substantial")
             (test-assert
              (not
               (project-adaptation--substantial-conversation-p
                (list :created-at       100
                      :last-activity-at 3699
                      :user-turns       20)))
              "recorded duration takes precedence over the legacy turn fallback")
             (test-assert
              (project-adaptation--substantial-conversation-p
               (list :created-at       nil
                     :last-activity-at nil
                     :user-turns
                     *project-adaptation-fallback-user-turns*))
              "twelve user turns qualify records without usable times")
             (let* ((resumed
                      (project-adaptation-tests--substantial-conversation
                       configuration "offered-resume"))
                    (output
                      (project-adaptation-tests--run-offer
                       configuration resumed (list :history-next :submit))))
               (test-assert
                (and
                 (not (probe-file notes))
                 (not
                  (project-adaptation-offer-due-p
                   configuration project-root (get-universal-time)))
                 (search "deferred for five days"
                         output))
                "resume offers support an explicit five-day deferral")
               (delete-file
                (configuration-project-adaptation-offers-path configuration))
               (let ((create-output
                       (project-adaptation-tests--run-offer
                        configuration resumed (list :submit))))
                 (test-assert
                  (and (probe-file notes)
                       (search "created" create-output))
                  "the default offer choice creates AUTOLITH.org"))
               (delete-file notes)
               (delete-file
                (configuration-project-adaptation-offers-path configuration))
               (let ((never-output
                       (project-adaptation-tests--run-offer
                        configuration
                        resumed
                        (list :history-next :history-next :submit))))
                 (test-assert
                  (and
                   (not (project-adaptation-offer-due-p
                         configuration project-root most-positive-fixnum))
                   (search "disabled permanently" never-output))
                  "the never choice permanently suppresses offers"))
               (delete-file
                (configuration-project-adaptation-offers-path configuration))
               (project-adaptation-tests--run-offer
                configuration resumed (list :escape))
               (test-assert
                (not (project-adaptation-offer-due-p
                      configuration project-root (get-universal-time)))
                "cancelling the offer applies the five-day cooldown"))
             (delete-file
              (configuration-project-adaptation-offers-path configuration))
             (project-adaptation-notes-create project-root)
             (test-assert
              (search (format nil "documentation, not~%executable Lisp")
                      (uiop:read-file-string notes))
              "the generated ledger explains its non-executable boundary")
             (with-open-file (stream notes
                                     :direction       ':output
                                     :if-exists       ':supersede
                                     :external-format ':utf-8)
               (write-string "adaptation-marker" stream))
             (project-adaptation-notes-create project-root)
             (test-assert
              (string= (uiop:read-file-string notes) "adaptation-marker")
              "creating notes never replaces an existing project file")
             (let* ((conversation
                      (conversation-create configuration
                                           :identifier "context"))
                    (request
                      (make-instance 'request-context
                                     :configuration   configuration
                                     :conversation    conversation
                                     :tool-namespaces #()))
                    (compaction-request
                      (make-instance 'request-context
                                     :configuration   configuration
                                     :conversation    conversation
                                     :tool-namespaces #()
                                     :compaction-p    t))
                    (contribution
                      (project-adaptation--context-contributor request))
                    (registration
                      (context--registration-find "project-adaptations")))
               (test-assert
                (and contribution
                     (search (namestring notes)
                             (context-contribution-evidence contribution))
                     (search "adaptation-marker"
                             (context-contribution-evidence contribution))
                     (eq (context-contribution-class contribution) ':mandatory))
                "an existing ledger activates scoped request-local advice")
               (test-assert
                (and registration
                     (eq (getf registration :source) ':built-in))
                "the project adaptation contributor has visible built-in provenance")
               (test-assert
                (null
                 (project-adaptation--context-contributor compaction-request))
                "AUTOLITH.org advice is absent from compaction context")
               (let* ((provider (provider-create configuration))
                      (normal-request
                        (provider-request-object provider conversation #()))
                      (compaction
                        (provider-request-object provider conversation #()
                                                 :compaction-p t)))
                 (test-assert
                  (search "adaptation-marker" (json-encode normal-request))
                  "normal provider requests receive project adaptation notes")
                 (test-assert
                  (not (search "adaptation-marker" (json-encode compaction)))
                  "compaction provider requests never receive project notes"))
               (test-assert (not (conversation-persisted-p conversation))
                            "project advice never creates conversation history"))))
      (platform-delete-directory-tree *platform* temporary-root
                                      :validate          t
                                      :if-does-not-exist ':ignore)))
  nil)
