;;;; -- Parent CLI Boundary Tests --

(in-package #:autolith-supervisor)

(serapeum/types:-> run-parent-cli-tests () boolean)
(defun run-parent-cli-tests ()
  "Exercise public command grammar, owner settings, and stable exit mapping."
  (let ((checks 0))
    (labels ((check (value)
               (incf checks)
               (unless value (error "Parent CLI boundary check ~D failed." checks)))

             (rejects (arguments)
               (handler-case
                   (progn (parent-cli-parse arguments) nil)
                 (parent-cli-error () t))))
      (dolist (case '((nil :session nil)
                      (("--fullscreen" "resume" "abc" "--immutable") :resume ("abc"))
                      (("auth" "custom" "device") :auth ("custom" "device"))
                      (("models" "--pristine") :models nil)
                      (("replay" "abc" "sequence" "4") :replay ("abc" "sequence" "4"))
                      (("fork" "abc" "turn" "2" "--id" "copy") :fork ("abc" "turn" "2"))
                      (("acp") :acp nil)
                      (("run-job" "--input" "job" "--output=result" "--events" "sexp") :run-job nil)
                      (("data" "export" "archive" "--all") :data ("export" "archive"))
                      (("data" "import" "archive" "--workspace" "dest") :data ("import" "archive"))
                      (("lg" "tell" "session" "hello") :localgroup ("tell" "session" "hello"))
                      (("localgroup" "attach" "--take-over" "session") :localgroup ("attach" "session"))
                      (("resume" "--" "--fullscreen") :resume ("--fullscreen"))))
        (destructuring-bind (arguments command operands) case
          (let ((request (parent-cli-parse arguments)))
            (check (eq (parent-cli-request-command request) command))
            (check (equal (parent-cli-request-arguments request) operands)))))
      (let ((request (parent-cli-parse '("--image=a,b" "resume" "x" "-i" "c"))))
        (check (equal (parent-cli-option request ':images) '("a" "b" "c"))))
      (check (equal (parent-cli-option (parent-cli-parse '("-ia,b")) ':images) '("a" "b")))
      (check (parent-cli-option (parent-cli-parse '("--from-source")) ':from-source))
      (check (handler-case
                 (progn (parent-cli-parse '("--localgroup-handoff" "ticket")) nil)
               (parent-cli-error (condition)
                 (eq (parent-cli-error-reason condition) ':unsupported-localgroup-handoff))))
      (check (handler-case
                 (progn (parent-cli-parse '("--from-source" "--image" "saved")) nil)
               (parent-cli-error (condition)
                 (eq (parent-cli-error-reason condition) ':conflicting-source-image))))
      (let ((request (parent-cli-parse '("--site-config-root" "--fullscreen"))))
        (check (equal (parent-cli-option request ':site-config-root) "--fullscreen"))
        (check (null (parent-cli-option request ':fullscreen))))
      (dolist (arguments '(("--unknown") ("--fullscreen=true") ("--image")
                           ("resume" "a" "b") ("replay") ("replay" "a" "bad" "1")
                           ("fork" "a" "turn") ("auth" "a" "b" "c")
                           ("run-job" "--input" "a")
                           ("run-job" "--input" "a" "--output" "b" "--events" "json")
                           ("data" "export" "a" "--all" "--workspace" "b")
                           ("localgroup" "attach" "--read-only" "--take-over")
                           ("localgroup" "tell") ("models" "extra")))
        (check (rejects arguments)))
      (check (= 0 (parent-cli-exit-code ':complete)))
      (check (= 130 (parent-cli-exit-code ':cancelled)))
      (check (= 76 (parent-cli-exit-code ':protocol-error)))
      (check (= 1 (parent-cli-exit-code ':unknown)))
      (let ((settings (parent-cli-settings
                       (parent-cli-parse '("--almighty" "--permissions" "sandbox"))
                       :preferences '(:fullscreen-p nil :terminal-theme :autolith)
                       :overrides   '(:model "explicit"))))
        (check (getf settings :fullscreen-p))
        (check (eq (getf settings :terminal-theme) ':almighty))
        (check (equal (getf settings :permission-mode) "sandbox"))
        (check (equal (getf settings :model) "explicit")))
      (check (null (getf (parent-cli-settings (parent-cli-parse '("--pristine"))
                                              :preferences '(:provider-extensions
                                                             ((:source :user :path "/private/provider.lisp"))))
                         :provider-extensions)))
      (let* ((root      (merge-pathnames
                         (format nil "autolith-parent-source-test-~A/"
                                 (ironclad:byte-array-to-hex-string (ironclad:random-data 16)))
                         (uiop:temporary-directory)))
             (builder   (merge-pathnames "script/build-active.lisp" root))
             (temporary (merge-pathnames "temporary/" root))
             (runtime   (or (uiop:getenv "AUTOLITH_SBCL")
                            (first (uiop:raw-command-line-arguments)))))
          (ensure-directories-exist builder)
          (with-open-file (stream builder :direction ':output)
            (write-line "(require :asdf)" stream)
            (write-line "(with-open-file (stream (first (uiop:command-line-arguments)) :direction :output :if-exists :supersede) (write-string \"fresh-source\" stream))"
                        stream))
          (let ((core (parent-cli-build-source-core root runtime temporary)))
            (check (equal (uiop:read-file-string core) "fresh-source"))
            (delete-file core))
          (with-open-file (stream builder :direction ':output :if-exists ':supersede)
            (write-line "(require :asdf)" stream)
            (write-line "(uiop:quit 9)" stream))
          (check (handler-case
                     (progn (parent-cli-build-source-core root runtime temporary) nil)
                   (parent-configuration-error () t))))
      (let ((path (merge-pathnames (format nil "autolith-parent-preferences-~36R.sexp"
                                          (random (expt 36 10)))
                                   (uiop:temporary-directory))))
        (unwind-protect
             (progn
               (with-open-file (stream path :direction ':output :if-exists ':supersede)
                 (write '(:preferences :version 8 :model "first" :model "second"
                          :unknown (:a 1 :b "data")) :stream stream :readably t))
               (let ((values (parent-cli-read-preferences path)))
                 (check (equal (getf values :model) "first"))
                 (check (equal (getf values :unknown) '(:a 1 :b "data"))))
               (dolist (invalid '("(:preferences :version 7)"
                                  "(:preferences :version 8) (:extra t)"
                                  "(:preferences :version 8 :x #.(error \"must not run\"))"
                                  "(:preferences :version 8 :x #100000000())"
                                  "#1=(:preferences :version 8 :x #1#)"))
                 (with-open-file (stream path :direction ':output :if-exists ':supersede)
                   (write-string invalid stream))
                 (check (handler-case (progn (parent-cli-read-preferences path) nil)
                          (parent-configuration-error () t)))))
          (when (probe-file path) (delete-file path)))))
    (format t "Parent CLI: ~D checks passed.~%" checks)
    t))

(serapeum/types:-> run-parent-cli-settings-tests () boolean)
(defun run-parent-cli-settings-tests ()
  "Check atomic owner preference writes, public value limits, and immutable rejection."
  (let* ((root (merge-pathnames (format nil "autolith-parent-settings-~36R/" (random (expt 36 10)))
                                (uiop:temporary-directory)))
         (path (merge-pathnames "preferences.sexp" root))
         (configuration
           (make-parent-configuration
            (list :version 1 :runtime "/runtime" :agent-core "/agent.core" :worker-core "/worker.core"
                  :source-root "/source/" :workspace "/workspace/" :private-root (namestring root)
                  :temporary-root "/temporary/" :runtime-roots '("/source/")
                  :profiles '((:id "fixture" :family :codex :model "fixture")) :profile-id "fixture"
                  :account-configuration (list :state-root (namestring root))
                  :settings              (copy-tree *parent-cli-setting-defaults*)
                  :preferences-path      (namestring path)))))
    (ensure-directories-exist path)
    (unwind-protect
         (progn
           (with-open-file (stream path :direction ':output)
             (write '(:preferences :version 8 :unknown ("keep") :compact-view-p t)
                    :stream stream :readably t))
           (parent-cli-store-settings configuration '(:compact-view-p nil :knowledge-mode "passive"))
           (assert (equal (getf (parent-cli-read-preferences path) :unknown) '("keep")))
           (assert (null (getf (parent-cli-read-preferences path) :compact-view-p)))
           (assert (equal (parent-configuration-get configuration ':knowledge-mode) "passive"))
           (assert (equal (gethash "knowledge-mode" (parent-cli-public-settings configuration)) "passive"))
           (let ((before (uiop:read-file-string path)))
             (assert (handler-case
                         (progn (parent-cli-store-settings configuration '(:provider-extensions nil)) nil)
                       (parent-cli-error () t)))
             (assert (equal before (uiop:read-file-string path)))
             (let ((settings (parent-configuration-get configuration ':settings)))
               (setf (getf settings :immutable-p) t))
             (assert (handler-case
                         (progn (parent-cli-store-settings configuration '(:knowledge-mode "off")) nil)
                       (parent-cli-error () t)))
             (assert (equal before (uiop:read-file-string path))))
           (format t "Parent CLI settings: 8 checks passed.~%")
           t)
      (when (probe-file path) (delete-file path))
      (let ((lock (merge-pathnames "preferences.lock" root)))
        (when (probe-file lock) (delete-file lock)))
      (uiop:delete-empty-directory root))))

(serapeum/types:-> run-parent-cli-launcher-tests () boolean)
(defun run-parent-cli-launcher-tests ()
  "Exercise public/default and internal-worker routing through the actual shell parser."
  (when (uiop:os-windows-p)
    (format t "Parent shell launcher routing skipped: POSIX shell unavailable.~%")
    (return-from run-parent-cli-launcher-tests t))
  (let ((script (namestring (asdf:system-relative-pathname "autolith-supervisor" "script/launcher-cli.sh")))
        (checks 0))
    (dolist (case '((nil "parent")
                    (("--fullscreen" "resume" "saved") "parent")
                    (("--site-config-root" "--worker") "parent")
                    (("--image" "--worker") "parent")
                    (("--pristine" "acp") "parent")
                    (("--worker") "agent")
                    (("--parent-agent") "agent")
                    (("--autolith-internal-image-commit-replay-probe" "path" "output") "agent")))
      (destructuring-bind (arguments expected) case
        (multiple-value-bind (output errors status)
            (uiop:run-program
             (append (list "bash" "-c"
                           "source \"$1\"; shift; autolith_launcher_parse source \"$@\"; if autolith_parent_cli_p \"${remaining_arguments[@]}\"; then printf parent; else printf agent; fi"
                           "autolith-parent-launch" script) arguments)
             :output ':string :error-output ':string :ignore-error-status t)
          (declare (ignore errors))
          (assert (and (zerop status) (string= output expected)))
          (incf checks))))
    (format t "Parent launcher: ~D routing checks passed.~%" checks)
    t))
