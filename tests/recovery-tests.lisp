(in-package #:autolith)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (fboundp 'recovery-context-create)
    (load (merge-pathnames "recovery/runtime.lisp"
                           (asdf:system-source-directory :autolith)))))


;;;; -- Recovery Runtime Tests --

(-> recovery-tests--write-form (pathname t) pathname)
(defun recovery-tests--write-form (pathname form)
  "Write one readable FORM to PATHNAME and return PATHNAME."
  (ensure-directories-exist pathname)
  (with-open-file (stream pathname
                          :direction ':output
                          :if-exists ':supersede
                          :if-does-not-exist ':create
                          :external-format ':utf-8)
    (let ((*print-readably* t))
      (prin1 form stream)
      (terpri stream)
      (finish-output stream)))
  pathname)

(-> recovery-tests--context (pathname) recovery-context)
(defun recovery-tests--context (root)
  "Return a recovery context isolated below test ROOT."
  (let ((state-root (merge-pathnames "state/autolith/" root)))
    (make-instance
     'recovery-context
     :source-root (asdf:system-source-directory :autolith)
     :generation-root (merge-pathnames "data/autolith/generations/" root)
     :worktree-root (merge-pathnames "data/autolith/recovery-worktrees/" root)
     :state-root state-root
     :current-pathname (merge-pathnames "current-generation.sexp" state-root))))

(-> recovery-tests--generation
    (pathname string string integer)
    recovery-generation)
(defun recovery-tests--generation (root identifier commit created-at)
  "Return a synthetic retained generation below ROOT."
  (let ((directory
          (merge-pathnames (format nil "~A/" identifier) root)))
    (make-instance
     'sbcl-generations:generation
     :identifier identifier
     :directory directory
     :core-pathname (merge-pathnames "autolith.core" directory)
     :temporary-core-pathname
     (merge-pathnames ".autolith.core.tmp" directory)
     :manifest-pathname (merge-pathnames "manifest.sexp" directory)
     :metadata (list :git-commit commit
                     :sbcl-version (lisp-implementation-version)
                     :operating-system (software-type)
                     :operating-system-version (software-version)
                     :architecture (machine-type))
     :created-at created-at)))

(-> recovery-tests--migration-record (list) list)
(defun recovery-tests--migration-record (entries)
  "Return one complete conversation identifier migration record for ENTRIES."
  (list :conversation-identifier-migration
        :version 1
        :status ':complete
        :updated-at (get-universal-time)
        :entries entries))

(-> recovery-tests--migration-pathname (recovery-context) pathname)
(defun recovery-tests--migration-pathname (context)
  "Return CONTEXT's durable conversation identifier migration pathname."
  (merge-pathnames "conversation-identifier-migration.sexp"
                   (recovery-context-state-root context)))

(-> recovery-tests--restore-environment (string (or null string)) null)
(defun recovery-tests--restore-environment (name previous)
  "Restore environment variable NAME to PREVIOUS."
  (if previous
      (platform-setenv name previous)
      (platform-unsetenv name))
  nil)

(-> test-recovery-xdg-directories () null)
(defun test-recovery-xdg-directories ()
  "Test recovery ignores empty and relative XDG directory values."
  (let* ((configuration (test-configuration))
         (root (test-configuration-root configuration))
         (source-root (asdf:system-source-directory :autolith))
         (saved-data (uiop:getenv "XDG_DATA_HOME"))
         (saved-state (uiop:getenv "XDG_STATE_HOME")))
    (unwind-protect
         (progn
           (dolist (invalid '("" "relative/xdg-home"))
             (platform-setenv "XDG_DATA_HOME" invalid)
             (platform-setenv "XDG_STATE_HOME" invalid)
             ;; The platform adapter applies the same rule, so its roots are
             ;; the host's defaults once the variables are ignored.
             (let ((context (recovery-context-create source-root))
                   (data-root (platform-application-root *platform* ':data))
                   (state-root (platform-application-root *platform* ':state)))
               (test-assert
                (equal (recovery-context-generation-root context)
                       (merge-pathnames "generations/" data-root))
                "recovery ignores an invalid XDG data home")
               (test-assert
                (equal (recovery-context-worktree-root context)
                       (merge-pathnames "recovery-worktrees/" data-root))
                "recovery worktrees use the default data home")
               (test-assert
                (equal (recovery-context-state-root context) state-root)
                "recovery ignores an invalid XDG state home")))
           (let ((data-home (merge-pathnames "xdg-data/" root))
                 (state-home (merge-pathnames "xdg-state/" root)))
             (platform-setenv "XDG_DATA_HOME" (namestring data-home))
             (platform-setenv "XDG_STATE_HOME" (namestring state-home))
             (let ((context (recovery-context-create source-root)))
               (test-assert
                (equal (recovery-context-generation-root context)
                       (merge-pathnames "autolith/generations/" data-home))
                "recovery accepts an absolute XDG data home")
               (test-assert
                (equal (recovery-context-state-root context)
                       (merge-pathnames "autolith/" state-home))
                "recovery accepts an absolute XDG state home"))))
      (recovery-tests--restore-environment "XDG_DATA_HOME" saved-data)
      (recovery-tests--restore-environment "XDG_STATE_HOME" saved-state)
      (platform-delete-directory-tree *platform* root
                                      :validate t
                                      :if-does-not-exist ':ignore)))
  nil)

(-> recovery-tests--write-executable (pathname string) pathname)
(defun recovery-tests--write-executable (pathname content)
  "Write executable CONTENT to PATHNAME and return PATHNAME."
  (ensure-directories-exist pathname)
  (with-open-file (stream pathname
                          :direction ':output
                          :if-exists ':supersede
                          :if-does-not-exist ':create
                          :external-format ':utf-8)
    (write-string content stream)
    (finish-output stream))
  (uiop:run-program (list "chmod" "755" (namestring pathname))
                    :output nil
                    :error-output nil)
  pathname)

(-> test-recovery-conversation-identifiers () null)
(defun test-recovery-conversation-identifiers ()
  "Test recovery-side short syntax and strict legacy identifier migration."
  (let* ((configuration (test-configuration))
         (root (test-configuration-root configuration))
         (context (recovery-tests--context root))
         (legacy "cb472f21-969d-48f5-9c1e-e793d19054b9")
         (unmapped "de916e8a-8227-443d-9e10-11794a62ebd6")
         (current "K8vQ2mp")
         (entry (list :old legacy
                      :new current
                      :created-at (get-universal-time)))
         (migration-pathname
           (recovery-tests--migration-pathname context)))
    (unwind-protect
         (progn
           (recovery-tests--write-form
            migration-pathname
            (recovery-tests--migration-record (list entry)))
           (test-assert
            (string= (recovery-conversation-identifier-normalize-display
                      "K-8vQ2mp")
                     current)
            "recovery normalizes displayed short conversation identifiers")
           (test-assert
            (string= (recovery-conversation-identifier-normalize-display
                      current)
                     current)
            "recovery preserves stored short conversation identifiers")
           (test-assert
            (string= (recovery-conversation-identifier-normalize-display
                      "K-8vQ2m0")
                     "K-8vQ2m0")
            "recovery does not normalize malformed displayed identifiers")
           (test-assert
            (equal (recovery-conversation-migration-entries context)
                   (list entry))
            "recovery reads a complete strict migration mapping")
           (test-assert
            (string= (recovery-conversation-identifier-resolve context legacy)
                     current)
            "recovery resolves a mapped legacy UUID")
           (test-assert
            (string= (recovery-conversation-identifier-resolve context unmapped)
                     unmapped)
            "recovery leaves an unmapped legacy UUID unchanged")
           (let ((arguments (list "--immutable" "resume" legacy)))
             (test-assert
              (equal (recovery-normalize-forwarded-arguments context arguments)
                     (list "--immutable" "resume" current))
              "recovery canonicalizes forwarded resume identifiers")
             (test-assert
              (string= (third arguments) legacy)
              "recovery does not mutate the caller's forwarded arguments"))
            (let ((arguments
                    (list "--permissions" "full"
                          "--localgroup-handoff" "/state/consumed.sexp"
                          "resume" legacy)))
              (test-assert
               (equal (recovery-normalize-forwarded-arguments context arguments)
                      (list "--permissions" "full" "resume" current))
               "recovery removes consumed localgroup handoff arguments")
              (test-assert
               (equal arguments
                      (list "--permissions" "full"
                            "--localgroup-handoff" "/state/consumed.sexp"
                            "resume" legacy))
               "handoff removal does not mutate the caller's arguments"))
           (recovery-tests--write-form
            migration-pathname
            (recovery-tests--migration-record
             (list entry
                   (list :old legacy
                         :new "96kpbjY"
                         :created-at (get-universal-time)))))
           (test-assert
            (handler-case
                (progn
                  (recovery-conversation-migration-entries context)
                  nil)
              (error ()
                t))
            "recovery rejects duplicate legacy migration sources")
           (recovery-tests--write-form
            migration-pathname
            (recovery-tests--migration-record
             (list entry
                   (list :old unmapped
                         :new current
                         :created-at (get-universal-time)))))
           (test-assert
            (handler-case
                (progn
                  (recovery-conversation-migration-entries context)
                  nil)
              (error ()
                t))
            "recovery rejects duplicate current migration destinations"))
      (platform-delete-directory-tree *platform* root
                                      :validate t
                                      :if-does-not-exist ':ignore)))
  nil)

(-> test-recovery-session-handoff () null)
(defun test-recovery-session-handoff ()
  "Test session fallback, capsule precedence, and history-floor propagation."
  (let* ((configuration (test-configuration))
         (root (test-configuration-root configuration))
         (context (recovery-tests--context root))
         (state-root (recovery-context-state-root context))
         (migration-pathname
           (recovery-tests--migration-pathname context))
         (session-pathname
           (merge-pathnames "recovery-session-pointers/launcher-test.sexp"
                            state-root))
         (capsule-pathname
           (merge-pathnames "crashes/capsule-test.sexp" state-root))
         (outside-pointer-pathname
           (merge-pathnames "outside-session-pointer.sexp" root))
         (legacy "cb472f21-969d-48f5-9c1e-e793d19054b9")
         (session-id "96kpbjY")
         (capsule-id "K8vQ2mp")
         (pointer-name "AUTOLITH_RECOVERY_SESSION_POINTER")
         (conversation-name "AUTOLITH_RECOVERY_CONVERSATION_ID")
         (sequence-name "AUTOLITH_RECOVERY_RENDERED_SEQUENCE")
         (history-floor-name
           "AUTOLITH_RECOVERY_HISTORY_FLOOR_SEQUENCE")
         (crash-pointer-name "AUTOLITH_CRASH_POINTER")
         (previous-pointer (uiop:getenv pointer-name))
         (previous-conversation (uiop:getenv conversation-name))
         (previous-sequence (uiop:getenv sequence-name))
         (previous-history-floor (uiop:getenv history-floor-name))
         (previous-crash-pointer (uiop:getenv crash-pointer-name)))
    (unwind-protect
         (let ((*error-output* (make-broadcast-stream)))
           (recovery-tests--write-form
            migration-pathname
            (recovery-tests--migration-record
             (list (list :old legacy
                         :new session-id
                         :created-at (get-universal-time)))))
           (recovery-tests--write-form
            session-pathname
            (list :recovery-session
                  :version 1
                  :conversation-id legacy
                  :rendered-sequence 41
                  :history-floor-sequence 17))
           (recovery-tests--write-form
            capsule-pathname
            (list :crash
                  :condition "fixture crash"
                  :conversation-id capsule-id
                  :rendered-sequence 73
                  :history-floor-sequence 29))
           (platform-setenv pointer-name (namestring session-pathname))
           (recovery-report-crash context :status nil :capsule nil)
           (test-assert
            (string= (uiop:getenv conversation-name) session-id)
            "recovery falls back to the per-launch session after a hard crash")
           (test-assert
            (string= (uiop:getenv sequence-name) "41")
            "session fallback propagates the rendered transcript cursor")
           (test-assert
            (string= (uiop:getenv history-floor-name) "17")
            "session fallback separately propagates the history floor")
           (recovery-report-crash
            context
            :status nil
            :capsule (namestring capsule-pathname))
           (test-assert
            (string= (uiop:getenv conversation-name) capsule-id)
            "a valid crash capsule takes precedence over the session pointer")
           (test-assert
            (string= (uiop:getenv sequence-name) "73")
            "the preferred crash capsule propagates its rendered cursor")
           (test-assert
            (string= (uiop:getenv history-floor-name) "29")
            "the preferred crash capsule separately propagates its history floor")
           (platform-unsetenv crash-pointer-name)
           (recovery-refresh-crash-context
            context (namestring capsule-pathname) :session-p nil)
           (test-assert
            (string= (uiop:getenv sequence-name) "73")
            "initial fallback setup preserves crash-capsule precedence")
           (recovery-tests--write-form
            session-pathname
            (list :recovery-session
                  :version 1
                  :conversation-id session-id
                  :rendered-sequence 81
                  :history-floor-sequence 31))
           (recovery-refresh-crash-context
            context (namestring capsule-pathname))
           (test-assert
            (and (string= (uiop:getenv conversation-name) session-id)
                 (string= (uiop:getenv sequence-name) "81")
                 (string= (uiop:getenv history-floor-name) "31"))
            "fallback retries refresh handoff state advanced by the failed child")
           (recovery-tests--write-form
            outside-pointer-pathname
            (list :recovery-session
                  :version 1
                  :conversation-id session-id
                  :rendered-sequence 41
                  :history-floor-sequence 17))
           (platform-setenv
            pointer-name
            (namestring outside-pointer-pathname))
           (test-assert
            (handler-case
                (progn
                  (recovery-read-session-pointer context)
                  nil)
              (error ()
                t))
            "recovery rejects a session pointer outside private state")
           (dolist (invalid-floor '("invalid" 0))
             (recovery-tests--write-form
              session-pathname
              (list :recovery-session
                    :version 1
                    :conversation-id session-id
                    :rendered-sequence 41
                    :history-floor-sequence invalid-floor))
             (platform-setenv pointer-name (namestring session-pathname))
             (test-assert
              (handler-case
                  (progn
                    (recovery-read-session-pointer context)
                    nil)
                (error ()
                  t))
              "recovery rejects malformed session pointer metadata")))
      (recovery-tests--restore-environment pointer-name previous-pointer)
      (recovery-tests--restore-environment
       conversation-name previous-conversation)
      (recovery-tests--restore-environment sequence-name previous-sequence)
      (recovery-tests--restore-environment
       history-floor-name previous-history-floor)
      (recovery-tests--restore-environment
       crash-pointer-name previous-crash-pointer)
      (platform-delete-directory-tree *platform* root
                                      :validate t
                                      :if-does-not-exist ':ignore)))
  nil)

(-> test-recovery-status-boundary () null)
(defun test-recovery-status-boundary ()
  "Test status 64 fallback within recovery and termination at the launcher."
  (test-assert
   (not (recovery-generation-terminal-status-p 64))
   "status 64 advances to another retained generation")
  (dolist (status '(0 130 143))
    (test-assert
     (recovery-generation-terminal-status-p status)
     "success and terminal signals stop retained-generation fallback"))
  (with-test-fixture (':posix-shell "the stable launcher preserving status 64")
    (recovery-tests--launcher-status-boundary))
  nil)

(-> recovery-tests--launcher-status-boundary () null)
(defun recovery-tests--launcher-status-boundary ()
  "Run the stable launcher over a fake SBCL that exits with status 64."
  (let* ((configuration (test-configuration))
         (root (test-configuration-root configuration))
         (source-root (asdf:system-source-directory :autolith))
         (fake-sbcl (merge-pathnames "bin/fake-sbcl" root))
         (log (merge-pathnames "calls.log" root))
         (data-home (merge-pathnames "data/" root))
         (state-home (merge-pathnames "state/" root)))
    (unwind-protect
         (progn
           (recovery-tests--write-executable
            fake-sbcl
            "#!/bin/sh
set -eu
printf 'call\\n' >> \"$AUTOLITH_TEST_LOG\"
exit 64
")
           (multiple-value-bind (output error-output status)
               (uiop:run-program
                (list "env"
                      (format nil "AUTOLITH_SBCL=~A"
                              (namestring fake-sbcl))
                      (format nil "AUTOLITH_TEST_LOG=~A"
                              (namestring log))
                      (format nil "XDG_DATA_HOME=~A"
                              (namestring data-home))
                      (format nil "XDG_STATE_HOME=~A"
                              (namestring state-home))
                      (namestring (merge-pathnames "bin/autolith" source-root))
                      "--from-source")
                :ignore-error-status t
                :output nil
                :error-output nil)
             (declare (ignore output error-output))
             (test-assert (= status 64)
                          "the stable launcher preserves active status 64"))
           (test-assert
            (= (count #\Newline (uiop:read-file-string log)) 1)
            "the stable launcher does not enter recovery for active status 64"))
      (platform-delete-directory-tree *platform* root
                                      :validate t
                                      :if-does-not-exist ':ignore)))
  nil)

(-> test-recovery-source-failure-reporting () null)
(defun test-recovery-source-failure-reporting ()
  "Test clean-source recovery reports and refreshes a secondary failure."
  (let* ((configuration (test-configuration))
         (root (test-configuration-root configuration))
         (context (recovery-tests--context root))
         (status 70)
         (refreshed-capsule :unset)
         (capsule "/state/crashes/original.sexp"))
    (unwind-protect
         (test-call-with-function-replacements
          (list
           (list 'recovery-boot-source
                 (lambda (active-context arguments)
                   (declare (ignore active-context arguments))
                   status))
           (list 'recovery-refresh-crash-context
                 (lambda (active-context current-capsule &key session-p)
                   (declare (ignore active-context session-p))
                   (setf refreshed-capsule current-capsule)
                   current-capsule)))
          (lambda ()
            (let ((output
                    (with-output-to-string (stream)
                      (let ((*error-output* stream))
                        (test-assert
                         (= (recovery-boot-source-with-report
                             context nil :capsule capsule)
                            70)
                         "secondary source failure preserves its status")))))
              (test-assert
               (and (search "status 70" output)
                    (equal refreshed-capsule capsule))
               "secondary source failure is reported and refreshes crash context"))
            (setf status 0
                  refreshed-capsule :unset)
            (let ((output
                    (with-output-to-string (stream)
                      (let ((*error-output* stream))
                        (test-assert
                         (zerop
                          (recovery-boot-source-with-report
                           context nil :capsule capsule))
                         "successful source recovery preserves its status")))))
              (test-assert
               (and (zerop (length output))
                    (eq refreshed-capsule :unset))
               "successful source recovery emits no failure report"))))
      (platform-delete-directory-tree *platform* root
                                      :validate t
                                      :if-does-not-exist ':ignore)))
  nil)

(-> test-recovery-generation-revision-boundary () null)
(defun test-recovery-generation-revision-boundary ()
  "Test automatic recovery revision matching and explicit rollback exceptions."
  (let* ((configuration (test-configuration))
         (root (test-configuration-root configuration))
         (context (recovery-tests--context root))
         (current-commit "1111111111111111111111111111111111111111")
         (stale-commit "2222222222222222222222222222222222222222")
         (stale
           (recovery-tests--generation root "stale" stale-commit 30))
         (current-a
           (recovery-tests--generation root "current-a" current-commit 20))
         (current-b
           (recovery-tests--generation root "current-b" current-commit 10))
         (selected stale)
         (generations (list stale current-a current-b)))
    (unwind-protect
         (let ((*error-output* (make-broadcast-stream)))
           (test-call-with-function-replacements
            (list
             (list 'recovery-generation-compatible-p
                   (lambda (generation)
                     (declare (ignore generation))
                     t))
             (list 'recovery-selected-generation
                   (lambda (active-context)
                     (declare (ignore active-context))
                     selected))
             (list 'recovery-generation-list
                   (lambda (active-context)
                     (declare (ignore active-context))
                     generations)))
            (lambda ()
              (test-assert
               (eq
                (recovery-selected-generation-or-fallback
                 context
                 :source-commit current-commit)
                current-a)
               "automatic recovery skips a selected cross-revision generation")
              (setf selected current-b)
              (test-assert
               (eq
                (recovery-selected-generation-or-fallback
                 context
                 :source-commit current-commit)
                current-b)
               "automatic recovery preserves a selected same-revision generation")
              (setf selected stale)
              (test-assert
               (eq (recovery-selected-generation-or-fallback context)
                   stale)
               "manual recovery may use an explicitly selected older generation")
              (setf generations (list stale))
              (test-assert
               (null
                (recovery-selected-generation-or-fallback
                 context
                 :source-commit current-commit))
               "automatic recovery returns to source when no matching core exists")))
           (test-call-with-function-replacements
            (list
             (list 'recovery-source-commit
                   (lambda (active-context)
                     (declare (ignore active-context))
                     current-commit)))
            (lambda ()
              (test-assert
               (string=
                (recovery-automatic-source-commit context nil "70")
                current-commit)
               "fatal automatic recovery is constrained to current source")
              (test-assert
               (string=
                (recovery-automatic-source-commit context nil "1")
                current-commit)
               "unexpected active exits are constrained to current source")
              (test-assert
               (null
                (recovery-automatic-source-commit context "stale" "70"))
               "an explicit generation bypasses the automatic revision filter")
              (test-assert
               (null
                (recovery-automatic-source-commit context nil "75"))
               "rollback status preserves a selected older generation")
              (test-assert
               (null
                (recovery-automatic-source-commit context nil nil))
               "manual recovery without a status preserves explicit selection")))
           (let ((selection-source :unset)
                 (boot-source :unset)
                 (selection-called-p nil))
             (test-call-with-function-replacements
              (list
               (list 'recovery-context-create
                     (lambda (source-root)
                       (declare (ignore source-root))
                       context))
               (list 'recovery-normalize-forwarded-arguments
                     (lambda (active-context arguments)
                       (declare (ignore active-context))
                       arguments))
               (list 'recovery-print-introduction
                     (lambda ()
                       nil))
               (list 'recovery-report-crash
                     (lambda (active-context
                              &key status capsule original-arguments)
                       (declare
                        (ignore active-context status capsule
                                original-arguments))
                       nil))
               (list 'recovery-source-commit
                     (lambda (active-context)
                       (declare (ignore active-context))
                       current-commit))
               (list 'recovery-selected-generation-or-fallback
                     (lambda (active-context &key source-commit)
                       (declare (ignore active-context))
                       (setf selection-called-p t
                             selection-source source-commit)
                       current-a))
               (list 'recovery-load-generation
                     (lambda (active-context pathname
                              &key expected-identifier)
                       (declare
                        (ignore active-context pathname expected-identifier))
                       stale))
               (list 'recovery-boot-with-source-fallback
                     (lambda (active-context generation arguments
                              &key capsule source-commit)
                       (declare
                        (ignore active-context generation arguments capsule))
                       (setf boot-source source-commit)
                       0)))
              (lambda ()
                (test-assert
                 (= (recovery-run
                     (list (namestring root) "--status" "70" "--"))
                    0)
                 "automatic fatal recovery reaches the boot boundary")
                (test-assert
                 (and (string= selection-source current-commit)
                      (string= boot-source current-commit))
                 "recovery threads its source revision through selection and boot")
                (setf selection-source :unset
                      boot-source :unset)
                (test-assert
                 (= (recovery-run
                     (list (namestring root) "--status" "75" "--"))
                    0)
                 "rollback recovery reaches the boot boundary")
                (test-assert
                 (and (null selection-source)
                      (null boot-source))
                 "rollback leaves selection and boot revision-unconstrained")
                (setf selection-called-p nil
                      boot-source :unset)
                (test-assert
                 (= (recovery-run
                     (list (namestring root)
                           "--generation" "stale"
                           "--status" "70"
                           "--"))
                    0)
                 "explicit generation recovery reaches the boot boundary")
                (test-assert
                 (and (not selection-called-p)
                      (null boot-source))
                 "explicit generation selection bypasses automatic revision policy"))))
           (let ((booted nil)
                 (generations (list stale current-a current-b)))
             (test-call-with-function-replacements
              (list
               (list 'recovery-generation-compatible-p
                     (lambda (generation)
                       (declare (ignore generation))
                       t))
               (list 'recovery-generation-list
                     (lambda (active-context)
                       (declare (ignore active-context))
                       generations))
               (list 'recovery-terminal-state-capture
                     (lambda ()
                       (make-instance 'recovery-terminal-state
                                      :settings nil)))
               (list 'recovery-terminal-state-restore
                     (lambda (state)
                       (declare (ignore state))
                       nil))
               (list 'recovery-refresh-crash-context
                     (lambda (active-context capsule &key session-p)
                       (declare (ignore active-context session-p))
                       capsule))
               (list 'recovery-boot-generation
                     (lambda (active-context generation arguments)
                       (declare (ignore active-context arguments))
                       (push (recovery-generation-identifier generation)
                             booted)
                       (if (eq generation current-a) 64 0))))
              (lambda ()
                (test-assert
                 (= (recovery-boot-with-fallback
                     context
                     current-a
                     nil
                     :source-commit current-commit)
                    0)
                 "same-revision retained fallback may reach a healthy core")
                (test-assert
                 (equal (nreverse booted) '("current-a" "current-b"))
                 "retained fallback never tries a cross-revision generation")))))
      (platform-delete-directory-tree *platform* root
                                      :validate t
                                      :if-does-not-exist ':ignore)))
  nil)

(-> run-recovery-tests () null)
(defun run-recovery-tests ()
  "Run recovery runtime regression tests."
  (test-recovery-xdg-directories)
  (test-recovery-conversation-identifiers)
  (test-recovery-session-handoff)
  (test-recovery-status-boundary)
  (test-recovery-source-failure-reporting)
  (test-recovery-generation-revision-boundary)
  nil)
