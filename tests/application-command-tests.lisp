(in-package #:autolith)

;;;; -- Application Command Protocol Tests --

(-> application-command-tests--command
    (&key (:definition-name symbol) (:name string) (:aliases list)
          (:busy-behavior keyword) (:terminal-behavior keyword)
          (:handler function))
    application-command)
(defun application-command-tests--command
    (&key definition-name name aliases (busy-behavior ':inspect)
          (terminal-behavior ':shared)
          (handler (lambda (application invocation)
                     (declare (ignore application invocation))
                     ':continue)))
  "Return one complete command fixture with the supplied policy."
  (application-command-create
   :definition-name definition-name
   :name name
   :aliases aliases
   :argument nil
   :description "test command"
   :tip "exists for command protocol tests."
   :busy-behavior busy-behavior
   :terminal-behavior terminal-behavior
   :handler handler))

(defvar *application-command-tests-semantic-call* nil
  "The arguments observed by the semantic command protocol fixture.")

(defvar *application-command-tests-interactive-p* nil
  "Whether the semantic command fixture observed interactive command context.")

(-> application-command-tests--macroexpand-error-p (list) boolean)
(defun application-command-tests--macroexpand-error-p (form)
  "Return true when macroexpanding FORM signals an error."
  (handler-case
      (progn
        (macroexpand-1 form)
        nil)
    (error ()
      t)))

(-> test-application-command-defining-form () null)
(defun test-application-command-defining-form ()
  "Test literal command metadata and handler declarations fail closed."
  (let ((snapshot (application-command--registry-snapshot)))
    (dolist
        (form
         '((define-application-command application-command-tests--missing-tip
               (:name "/missing-tip"
                :argument nil
                :description "missing tip"
                :busy-behavior :inspect
                :terminal-behavior :shared)
               (application invocation)
             (declare (ignore application invocation))
             :continue)
           (define-application-command application-command-tests--blank-tip
               (:name "/blank-tip"
                :argument nil
                :description "blank tip"
                :tip "   "
                :busy-behavior :inspect
                :terminal-behavior :shared)
               (application invocation)
             (declare (ignore application invocation))
             :continue)
           (define-application-command application-command-tests--bad-busy
               (:name "/bad-busy"
                :argument nil
                :description "bad busy policy"
                :tip "has invalid policy."
                :busy-behavior :immediate
                :terminal-behavior :shared)
               (application invocation)
             (declare (ignore application invocation))
             :continue)
           (define-application-command application-command-tests--bad-terminal
               (:name "/bad-terminal"
                :argument nil
                :description "bad terminal policy"
                :tip "has invalid policy."
                :busy-behavior :inspect
                :terminal-behavior :sometimes)
               (application invocation)
             (declare (ignore application invocation))
             :continue)
            (define-application-command :keyword-definition
                (:name "/keyword"
                 :argument nil
                 :description "keyword identity"
                 :tip "has invalid identity."
                 :busy-behavior :inspect
                 :terminal-behavior :shared)
                (application invocation)
              (declare (ignore application invocation))
              :continue)
             (define-application-command application-command-tests--bad-callable
                 (:name "/bad-callable"
                  :argument "VALUE"
                  :description "invalid callable marker"
                  :tip "has invalid callable metadata."
                  :busy-behavior :inspect
                  :terminal-behavior :shared
                  :callable :yes)
                 (application value)
               (declare (ignore application value))
               :continue)
             (define-application-command application-command-tests--legacy-options
                 (:name "/legacy-options"
                  :description "invalid legacy static options"
                  :tip "has incompatible static option metadata."
                  :busy-behavior :inspect
                  :terminal-behavior :shared
                  :static-options ("on" "off"))
                 (application invocation)
               (declare (ignore application invocation))
               :continue)
             (define-application-command application-command-tests--duplicate-options
                 (:name "/duplicate-options"
                  :argument "[on|off]"
                  :description "duplicated finite options"
                  :tip "has duplicated option metadata."
                  :busy-behavior :inspect
                  :terminal-behavior :shared
                  :callable t
                  :static-options ("on" "off"))
                 (application &optional value)
               (declare (ignore application value))
               :continue)))
      (test-assert
       (application-command-tests--macroexpand-error-p form)
       "invalid command metadata fails during macro expansion"))
    (test-assert
     (equal snapshot (application-command--registry-snapshot))
     "macro expansion never mutates the live command registry"))
    (test-assert
     (handler-case
         (progn
           (application-command-create
            :definition-name 'application-command-tests--invalid-static-constructor
            :name "/invalid-static-constructor"
            :aliases nil
            :description "invalid static constructor"
            :tip "tests constructor validation."
            :busy-behavior ':inspect
            :terminal-behavior ':shared
            :lambda-list '()
            :callable-p t
            :static-options '("on" "off")
            :handler (lambda (application)
                       (declare (ignore application))
                       ':continue))
           nil)
       (configuration-error ()
         t))
     "programmatic construction enforces the finite-option arity invariant")
  nil)

(-> test-application-command-semantic-calls () null)
(defun test-application-command-semantic-calls ()
  "Test callable commands retain ordinary lambda-list and slash semantics."
  (let ((snapshot (application-command--registry-snapshot)))
    (unwind-protect
         (progn
           (eval
              '(define-application-command application-command-tests--semantic
                   (:name "/semantic"
                    :argument "REQUIRED [OPTIONAL]"
                    :description "exercise semantic command arguments"
                    :tip "exists for semantic argument tests."
                    :busy-behavior :inspect
                    :terminal-behavior :exclusive-without-arguments
                    :callable t)
                   (application required &optional (optional "default"))
                (declare (ignore application))
                (setf *application-command-tests-semantic-call*
                      (list required optional)
                      *application-command-tests-interactive-p*
                      *application-command-interactive-p*)
                :continue))
            (let ((command (application-command-find "/semantic")))
              (test-assert
               (and command
                    (application-command-callable-p command)
                    (equal (application-command-lambda-list command)
                           '(required &optional (optional "default"))))
               "callable command derives its contract from the handler lambda list")
              (let ((invocation
                      (application-operation--command-invocation command nil)))
                (test-assert
                 (and (zerop
                       (application-command-invocation-supplied-argument-count
                        invocation))
                      (eq (application-command-busy-action command invocation)
                          ':execute)
                      (application-command-terminal-owner-p command invocation))
                 "canonical calls without arguments retain argument-free policy"))
              (dolist (argument '(nil ""))
                (let ((invocation
                        (application-operation--command-invocation
                         command (list argument))))
                  (test-assert
                   (and (= 1
                           (application-command-invocation-supplied-argument-count
                            invocation))
                        (equal (application-command-invocation-arguments invocation)
                               (list argument))
                        (eq (application-command-busy-action command invocation)
                            ':hold)
                        (not
                         (application-command-terminal-owner-p command invocation)))
                   "canonical explicit NIL and empty strings count as supplied")))
             (let ((invocation
                     (application-command-invocation-parse
                      "/semantic \"alpha beta\"")))
               (test-assert
                (equal (application-command-invocation-arguments invocation)
                       '("alpha beta"))
                "slash parsing preserves whitespace inside double quotes")
               (application-command-execute command nil invocation)
               (test-assert
                (equal *application-command-tests-semantic-call*
                       '("alpha beta" "default"))
                "quoted slash arguments reach the canonical Lisp handler"))
             (let ((invocation
                     (application-command-invocation-parse
                      "/semantic \"C:\\temp\\new file\"")))
               (test-assert
                (equal (application-command-invocation-arguments invocation)
                       '("C:\\temp\\new file"))
                "slash parsing preserves literal backslashes inside quotes")
               (application-command-execute command nil invocation)
               (test-assert
                (equal *application-command-tests-semantic-call*
                       '("C:\\temp\\new file" "default"))
                "quoted backslashes reach the canonical Lisp handler"))
             (dolist (option '("alpha beta" "a\"b" "C:\\temp"))
               (test-assert
                (equal (application-command--tokens
                        (application-command--slash-option-token option))
                       (list option))
                "slash option rendering round-trips parser-significant characters"))
             (test-assert
              (not *application-command-tests-interactive-p*)
              "direct command execution remains noninteractive by default")
             (setf *application-command-tests-interactive-p* nil)
             (application-command (make-instance 'application) "/semantic slash")
             (test-assert
              (and *application-command-tests-interactive-p*
                   (equal *application-command-tests-semantic-call*
                          '("slash" "default")))
              "slash dispatch dynamically enables interactive command context")
             (let ((invocation
                     (make-instance
                      'application-command-invocation
                      :input "(semantic nil nil)"
                      :name "/semantic"
                      :remainder "nil nil"
                      :argument "nil"
                      :arguments '(nil nil)
                      :supplied-argument-count 2
                      :command command)))
               (application-command-execute command nil invocation)
               (test-assert
                (equal *application-command-tests-semantic-call* '(nil nil))
                "explicit NIL arguments remain explicitly supplied"))
             (test-assert
              (handler-case
                  (progn
                    (application-command-execute
                     command nil
                     (application-operation--command-invocation command nil))
                    nil)
                (program-error (condition)
                  (null (find-restart 'supply-arguments condition))))
              "noninteractive omission signals PROGRAM-ERROR without prompting")
             (let ((restart-seen-p nil)
                   (*application-command-interactive-p* t))
               (handler-bind
                   ((program-error
                      (lambda (condition)
                        (let ((restart
                                (find-restart 'supply-arguments condition)))
                          (setf restart-seen-p (not (null restart)))
                          (when restart
                            (invoke-restart restart "recovered" nil))))))
                 (application-command-execute
                  command nil
                  (application-operation--command-invocation command nil)))
               (test-assert
                (and restart-seen-p
                     (equal *application-command-tests-semantic-call*
                            '("recovered" nil)))
                "interactive arity recovery retries with replacement arguments"))))
      (application-command--registry-restore snapshot)))
  nil)

(-> test-application-command-registry () null)
(defun test-application-command-registry ()
  "Test ordered replacement, layering, aliases, and collision atomicity."
  (let ((snapshot (application-command--registry-snapshot)))
    (unwind-protect
         (progn
           (application-command--registry-restore nil)
           (let* ((alpha
                    (application-command-tests--command
                     :definition-name
                     'application-command-tests--alpha
                     :name "/alpha"
                     :aliases '("/a")))
                  (beta
                    (application-command-tests--command
                     :definition-name
                     'application-command-tests--beta
                     :name "/beta"
                     :aliases nil))
                  (renamed
                    (application-command-tests--command
                     :definition-name
                     'application-command-tests--alpha
                     :name "/renamed"
                     :aliases '("/r"))))
             (register-application-command alpha :source ':runtime)
             (register-application-command beta :source ':runtime)
             (register-application-command renamed :source ':runtime)
             (test-assert
              (equal (mapcar #'application-command-name
                             (application-command-list))
                     '("/renamed" "/beta"))
              "redefining one command preserves its registry position")
             (test-assert
              (and (null (application-command-find "/alpha"))
                   (null (application-command-find "/a"))
                   (eq (application-command-find "/R") renamed))
              "renaming a command removes stale names and resolves aliases")
             (let ((shadow
                     (application-command-tests--command
                      :definition-name
                      'application-command-tests--shadow
                      :name "/renamed"
                      :aliases '("/shadow"))))
               (register-application-command shadow :source ':user)
               (test-assert
                (eq (application-command-find "/renamed") shadow)
                "a later layer shadows the same canonical command")
               (unregister-application-command
                'application-command-tests--shadow
                :source ':user)
               (test-assert
                (eq (application-command-find "/renamed") renamed)
                "removing a layer reveals the preceding command"))
             (let* ((before (application-command--registry-snapshot))
                    (collision
                      (application-command-tests--command
                       :definition-name
                       'application-command-tests--collision
                       :name "/collision"
                       :aliases '("/beta"))))
               (test-assert
                (handler-case
                    (progn
                      (register-application-command
                       collision
                       :source ':runtime)
                      nil)
                  (configuration-error ()
                    t))
                "an effective alias collision is rejected")
               (test-assert
                (equal before (application-command--registry-snapshot))
                "a rejected collision leaves every registry projection unchanged"))))
      (application-command--registry-restore snapshot)))
  nil)

(-> test-application-command-policies () null)
(defun test-application-command-policies ()
  "Test invocation parsing and invocation-sensitive command policies."
  (let ((snapshot (application-command--registry-snapshot)))
    (unwind-protect
         (progn
           (application-command--registry-restore nil)
           (dolist
               (command
                (list
                 (application-command-tests--command
                   :definition-name 'application-command-tests--peek
                   :name "/peek"
                   :aliases '("/i")
                  :busy-behavior ':inspect)
                 (application-command-tests--command
                  :definition-name 'application-command-tests--hold
                  :name "/hold"
                  :aliases nil
                  :busy-behavior ':hold
                  :terminal-behavior ':exclusive)
                 (application-command-tests--command
                  :definition-name 'application-command-tests--conditional
                  :name "/conditional"
                  :aliases nil
                  :busy-behavior ':cancel
                  :terminal-behavior ':exclusive-without-arguments)))
             (register-application-command command :source ':runtime))
           (let* ((inspection
                    (application-command-invocation-parse "  /I  "))
                  (inspection-with-argument
                     (application-command-invocation-parse
                      "/peek alpha beta"))
                  (held
                    (application-command-invocation-parse "/hold"))
                  (conditional
                    (application-command-invocation-parse "/conditional value")))
             (test-assert
              (and
               (string= (application-command-invocation-name inspection) "/i")
               (string=
                (application-command-name
                 (application-command-invocation-command inspection))
                 "/peek")
               (string=
                (application-command-invocation-remainder
                 inspection-with-argument)
                "alpha beta")
               (string=
                (application-command-invocation-argument
                 inspection-with-argument)
                "alpha"))
              "command parsing preserves the full remainder and resolves aliases")
             (test-assert
              (and
               (eq (application-command-busy-action
                    (application-command-invocation-command inspection)
                    inspection)
                   ':execute)
               (eq (application-command-busy-action
                    (application-command-invocation-command
                     inspection-with-argument)
                    inspection-with-argument)
                   ':hold)
               (eq (application-command-busy-action
                    (application-command-invocation-command held)
                    held)
                   ':hold)
               (eq (application-command-busy-action
                    (application-command-invocation-command conditional)
                    conditional)
                   ':cancel))
              "busy behavior is declared by the command and refined by invocation")
             (test-assert
              (and
               (application-command-terminal-owner-p
                (application-command-invocation-command held)
                held)
               (not
                (application-command-terminal-owner-p
                 (application-command-invocation-command conditional)
                 conditional)))
              "terminal ownership follows each command's declared policy")))
      (application-command--registry-restore snapshot)))
  nil)

(-> test-built-in-application-command-policies () null)
(defun test-built-in-application-command-policies ()
  "Test representative built-ins follow active-turn and terminal policy."
  (dolist
      (case
       '(("/help" :execute)
         ("/cwd /tmp" :hold)
         ("/model gpt-5.6-sol" :apply)
         ;; Detaching exists to leave while work runs; it must never wait
         ;; for the idle queue.
         ("/detach" :execute)
         ;; Vault commands manage the follow-up queue itself, so they act
         ;; immediately rather than waiting behind that queue.
         ("/vault-restore" :execute)
         ("/vault-discard" :execute)
         ("/quit" :cancel)))
    (destructuring-bind (input expected) case
      (let* ((invocation (application-command-invocation-parse input))
             (command (application-command-invocation-command invocation)))
        (test-assert
         (eq (application-command-busy-action command invocation) expected)
         (format nil "~A has active-turn behavior ~S" input expected)))))
  (dolist
      (case
       '(("/model" t)
         ("/effort high" nil)
         ("/resume K-8vQ2mp" nil)
         ("/compact" nil)))
    (destructuring-bind (input expected) case
      (let* ((invocation (application-command-invocation-parse input))
             (command (application-command-invocation-command invocation)))
        (test-assert
         (eq (not
              (null
               (application-command-terminal-owner-p command invocation)))
             expected)
         (format nil "~A has terminal ownership ~S" input expected)))))
  nil)

(-> test-built-in-application-command-calls () null)
(defun test-built-in-application-command-calls ()
  "Test built-ins expose ordinary lambda lists without implicit picker calls."
  (test-assert
   (every #'application-command-callable-p
          (application-command-list))
   "every built-in command uses ordinary Common Lisp call semantics")
  (dolist
      (case
       '(("/help" ())
         ("/resume" (&optional (identifier nil identifier-supplied-p)))
         ("/cwd" (&optional (pathname "")))
         ("/trace" (&optional mode))
         ("/timestamps" (&optional mode))
         ("/ste" (&optional mode))
         ("/titles" (&optional mode))
         ("/hurry-up" (&optional mode))
         ("/papercut" (&optional identifier))
         ("/papercut-close" (&optional identifier))))
    (destructuring-bind (name lambda-list) case
      (let ((command (application-command-find name)))
        (test-assert
         (and command
              (equal (application-command-lambda-list command) lambda-list))
         (format nil "~A derives its slash contract from its Lisp lambda list"
                 name)))))
  (let ((command (application-command-find "/ste")))
    (test-assert
     (and (equal (application-command-static-options command) '("on" "off"))
          (string= (application-command-argument command) "[on|off]"))
     "finite options derive the optional argument hint from one metadata list"))
  (let* ((objective "fix argument mismatches without crashing the application")
         (invocation
           (application-command-invocation-parse
            (format nil "/goal ~A" objective))))
    (test-assert
     (and (equal (application-command-invocation-arguments invocation)
                 (list objective))
          (string= (application-command-invocation-argument invocation)
                   objective))
     "single-argument free-form commands preserve their complete remainder"))

  (let* ((objective "Fix the \"widget=")
         (invocation
           (application-command-invocation-parse
            (format nil "/goal ~A" objective))))
    (test-assert
     (equal (application-command-invocation-arguments invocation)
            (list objective))
     "raw-remainder commands do not tokenize quotation marks"))
  (let ((application (make-instance 'application))
        (objective "fix argument mismatches without crashing the application"))
    (test-call-with-function-replacements
     (list
      (list 'application-present
            (lambda (candidate entry)
              (declare (ignore candidate entry))
              t))
      (list 'application--record-goal
            (lambda (candidate)
              (declare (ignore candidate))
              nil))
      (list 'application--start-goal-work
            (lambda (candidate)
              (declare (ignore candidate))
              nil)))
     (lambda ()
       (test-assert
        (eq (application-command
             application
             (format nil "/goal ~A" objective))
            ':continue)
        "a multiword goal executes as one free-form command argument")
       (test-assert
        (string= (getf (application-goal application) :objective) objective)
        "the complete multiword goal becomes the session objective"))))
  (let* ((configuration (test-configuration))
         (root          (test-configuration-root configuration))
         (application   (make-instance 'application :configuration configuration)))
    (unwind-protect
         (test-call-with-function-replacements
          (list
           (list 'application-present
                 (lambda (candidate entry)
                   (declare (ignore candidate entry))
                   t)))
          (lambda ()
            (dolist (name '("cwd" "trace" "timestamps" "ste" "titles"
                            "hurry-up" "papercut" "papercut-close"))
              (test-assert
               (eq (application-command application (format nil "/~A" name))
                   ':continue)
               (format nil "/~A accepts its omitted optional argument" name))
              (test-assert
               (null (application-operation-call application name))
               (format nil "(~A) accepts its omitted optional argument" name)))))
      (platform-delete-directory-tree *platform* root :validate t :if-does-not-exist ':ignore)))
  (dolist (case '(("/auth grok device typo" 4)
                  ("/mcp refresh typo" 3)
                  ("/permissions auto garbage" 3)))
    (destructuring-bind (input token-count) case
      (test-assert
       (= (length (application-command--tokens input)) token-count)
       (format nil "~A tokenizes every supplied slash argument" input))
      (let* ((invocation (application-command-invocation-parse input))
             (command (application-command-invocation-command invocation)))
        (test-assert
         (handler-case
             (progn
               (application-command-execute command nil invocation)
               nil)
           (configuration-error ()
             t))
         (format nil "~A reports excess slash arguments during guarded dispatch"
                 input)))))
  (test-assert
   (handler-case
       (progn
         (application-command-invocation-parse "/cwd \"unfinished")
         nil)
     (configuration-error ()
       t))
   "slash parsing rejects unterminated quoted arguments")
  (let ((application (make-instance 'application)))
    (test-assert
     (handler-case
         (progn
           (application--builtin-help-command application :extra)
           nil)
       (program-error ()
         t))
     "an argument-free built-in rejects extra Lisp arguments")
    (setf (application-project-adaptation-offer-p application) t)
    (let ((*application-command-interactive-p* t))
      (test-assert
       (eq (application--builtin-model-command application nil) ':continue)
       "an explicit NIL optional argument never invokes its picker"))
    (let ((*application-command-interactive-p* nil))
      (test-assert
       (eq (application--builtin-resume-command application) ':continue)
       "noninteractive omission remains nonmodal"))
    (test-assert
     (application-project-adaptation-offer-p application)
     "nonmodal resume calls do not consume the interactive startup offer"))
  nil)

(-> test-application-codex-fast-mode-command () null)
(defun test-application-codex-fast-mode-command ()
  "Test /fast status, persistence, runtime installation, and validation."
  (let* ((configuration
           (configuration-with-codex-fast-mode (test-configuration) nil))
         (root (test-configuration-root configuration))
         (application (make-instance 'application
                                     :configuration configuration))
         (previous-fast-mode (uiop:getenv "AUTOLITH_CODEX_FAST_MODE"))
         (presented nil)
         (persisted nil)
         (installed nil)
         (published-count 0))
    (unwind-protect
         (progn
           (platform-unsetenv "AUTOLITH_CODEX_FAST_MODE")
           (test-call-with-function-replacements
            (list
             (list
              'application-present
              (lambda (candidate entry)
                (declare (ignore candidate))
                (push entry presented)
                t))
             (list
              'preferences-set-codex-fast-mode
              (lambda (candidate enabled-p)
                (push (list enabled-p
                            (configuration-codex-fast-mode-p candidate))
                      persisted)
                nil))
             (list
              'application--install-configuration
              (lambda (candidate replacement &key conversation)
                (declare (ignore conversation))
                (setf (application-configuration candidate) replacement)
                (push (configuration-codex-fast-mode-p replacement) installed)
                nil))
             (list
              'application-publish-recovery-session
              (lambda (candidate)
                (declare (ignore candidate))
                (incf published-count)
                nil)))
            (lambda ()
              (test-assert
               (eq (application--builtin-fast-command application) ':continue)
               "/fast status completes through the canonical command")
              (test-assert
               (search "Codex Fast mode is off." (first presented))
               "/fast reports the disabled state")
              (test-assert
               (eq (application--builtin-fast-command application "status")
                   ':continue)
               "/fast status accepts its explicit spelling")
              (let ((codex-configuration
                      (application-configuration application)))
                (setf (application-configuration application)
                      (configuration-with-model codex-configuration "grok-4.5"))
                (application--builtin-fast-command application "status")
                (test-assert
                 (and (search "preference is off" (first presented))
                      (search "standard path" (first presented)))
                 "/fast identifies an inactive saved preference outside Codex")
                (setf (application-configuration application)
                      codex-configuration))
              (test-assert
               (and (null persisted)
                    (null installed)
                    (zerop published-count))
               "/fast status has no persistence or runtime side effects")
              (test-assert
               (eq (application--builtin-fast-command application "ON")
                   ':continue)
               "/fast on completes through the canonical command")
              (test-assert
               (and (configuration-codex-fast-mode-p
                     (application-configuration application))
                    (equal (first persisted) '(t t))
                    (eq (first installed) t)
                    (= published-count 1)
                    (search "enabled and saved" (first presented)))
               "/fast on persists and installs the enabled state")
              (test-assert
               (eq (application--builtin-fast-command application "off")
                   ':continue)
               "/fast off completes through the canonical command")
              (test-assert
               (and (not
                     (configuration-codex-fast-mode-p
                      (application-configuration application)))
                    (equal (first persisted) '(nil nil))
                    (null (first installed))
                    (= published-count 2)
                    (search "disabled and saved" (first presented)))
               "/fast off persists and installs the disabled state")
              (test-assert
               (handler-case
                   (progn
                     (application--builtin-fast-command application "turbo")
                     nil)
                 (configuration-error ()
                   t))
               "/fast rejects unsupported modes")
              (platform-setenv "AUTOLITH_CODEX_FAST_MODE" "off")
              (application--builtin-fast-command application "status")
              (test-assert
               (search "AUTOLITH_CODEX_FAST_MODE controls it" (first presented))
               "/fast status reports the process environment override")
              (test-assert
               (handler-case
                   (progn
                     (application--builtin-fast-command application "on")
                     nil)
                 (configuration-error ()
                   t))
               "/fast cannot supersede the process environment override")
              (test-assert
               (and (= (length persisted) 2)
                    (= (length installed) 2)
                    (= published-count 2))
               "invalid or environment-controlled /fast input has no side effects"))))
      (if previous-fast-mode
          (platform-setenv "AUTOLITH_CODEX_FAST_MODE" previous-fast-mode)
          (platform-unsetenv "AUTOLITH_CODEX_FAST_MODE"))
      (platform-delete-directory-tree *platform* root
                                      :validate t
                                      :if-does-not-exist ':ignore)))
  nil)

(defclass application-authentication-test-provider (model-provider)
  ((stream
    :initform nil
    :accessor application-authentication-test-provider-stream
    :type (option stream)
    :documentation "The direct stream received by the test authenticator.")
   (input-file-descriptor
    :initform nil
    :accessor application-authentication-test-provider-input-file-descriptor
    :type (option integer)
    :documentation "The dynamically supplied descriptor for hidden input.")
   (styled-p
    :initform nil
    :accessor application-authentication-test-provider-styled-p
    :type boolean
    :documentation "Whether semantic prompt styling was enabled for authentication.")
   (open-browser-p
    :initform nil
    :accessor application-authentication-test-provider-open-browser-p
    :type boolean
    :documentation "Whether the test authenticator was asked to open a browser.")
   (read-input-p
    :initarg :read-input-p
    :initform nil
    :reader application-authentication-test-provider-read-input-p
    :type boolean
    :documentation "Whether authentication reads one hidden API-key input line.")
   (activity-callback
    :initarg :activity-callback
    :initform nil
    :reader application-authentication-test-provider-activity-callback
    :type (option function)
    :documentation "An optional callback invoked while authentication owns the terminal.")
   (input
    :initform nil
    :accessor application-authentication-test-provider-input
    :type (option string)
    :documentation "The hidden input read by the test authenticator."))
  (:documentation "A provider recording one application authentication call."))

(defmethod provider-authenticate
    ((provider application-authentication-test-provider)
     &key stream open-browser-p)
  "Write immediate public instructions for PROVIDER to STREAM."
  (setf (application-authentication-test-provider-stream provider) stream
        (application-authentication-test-provider-input-file-descriptor provider)
        *api-key-input-file-descriptor*
        (application-authentication-test-provider-styled-p provider)
        *api-key-output-styled-p*
        (application-authentication-test-provider-open-browser-p provider)
        open-browser-p)
  (format stream "Open the provider login page now.~%")
  (finish-output stream)
  (let ((callback
          (application-authentication-test-provider-activity-callback provider)))
    (when callback
      (funcall callback)))
  (when (application-authentication-test-provider-read-input-p provider)
    (setf (application-authentication-test-provider-input provider)
          (api-key-read-hidden "Example" :stream stream)))
  "Provider authentication was saved.")

(defclass application-authentication-test-localgroup-terminal (localgroup-terminal)
  ((events
    :initarg :events
    :accessor application-authentication-test-localgroup-terminal-events
    :type list
    :documentation "The semantic authentication input events still queued."))
  (:documentation "A localgroup terminal with scripted authentication input."))

(defmethod terminal-read-event
    ((terminal application-authentication-test-localgroup-terminal))
  "Read TERMINAL's next scripted authentication event."
  (or (pop (application-authentication-test-localgroup-terminal-events terminal))
      ':end-of-input))

(-> test-terminal-authentication-streams () null)
(defun test-terminal-authentication-streams ()
  "Test terminal classes select authentication streams through the protocol."
  (let ((input (make-string-input-stream ""))
        (output (make-string-output-stream)))
    (let ((*standard-input* input)
          (*standard-output* output))
      (multiple-value-bind
            (observed-input observed-output stop-ui-p echo-disabled-p descriptor)
          (terminal-authentication-streams (make-instance 'terminal))
        (test-assert
         (and (eq observed-input input)
              (eq observed-output output)
              stop-ui-p
              (not echo-disabled-p)
              (null descriptor))
         "base terminals use process streams and stop the UI"))))
  (let* ((input (make-string-input-stream ""))
         (output (make-string-output-stream))
         (terminal
           (make-instance 'stream-terminal
                          :input-stream input
                          :output-stream output
                          :input-file-descriptor 7)))
    (multiple-value-bind
          (observed-input observed-output stop-ui-p echo-disabled-p descriptor)
        (terminal-authentication-streams terminal)
      (test-assert
       (and (eq observed-input input)
            (eq observed-output output)
            stop-ui-p
            (not echo-disabled-p)
            (= descriptor 7))
       "stream terminals expose their direct authentication streams")))
  (let ((terminal (localgroup-terminal-create nil)))
    (multiple-value-bind
          (input output stop-ui-p echo-disabled-p descriptor)
        (terminal-authentication-streams terminal)
      (test-assert
       (and (typep input 'application-authentication-input-stream)
            (typep output 'application-authentication-output-stream)
            (not stop-ui-p)
            echo-disabled-p
            (null descriptor))
       "localgroup terminals adapt semantic events without stopping the UI")))
  nil)

(-> test-application-authentication-command () null)

(defun test-application-authentication-command ()
  "Test /auth and (auth) share selection, explicit naming, and direct output."
  (let ((application (make-instance 'application))
        (picked-p nil)
        (authenticated-provider nil)
        (authenticated-method nil))
    (test-call-with-function-replacements
     (list
      (list 'application--pick-authentication-provider
            (lambda (candidate) (declare (ignore candidate)) (setf picked-p t) "grok"))
      (list 'application-authenticate
            (lambda (candidate provider-name &optional method)
              (declare (ignore candidate))
              (setf authenticated-provider provider-name
                    authenticated-method method)
              nil)))
     (lambda ()
       (let ((*application-command-interactive-p* t))
         (test-assert
          (eq (application--builtin-authentication-command application) ':continue)
          "argument-free auth completes through the canonical command")
         (test-assert (and picked-p (string= authenticated-provider "grok"))
          "argument-free auth picks and authenticates one provider")
         (setf picked-p nil
               authenticated-provider nil
               authenticated-method nil)
         (test-assert
          (eq (application--builtin-authentication-command application "anthropic")
              ':continue)
          "named auth completes through the canonical command")
         (test-assert
          (and (not picked-p) (string= authenticated-provider "anthropic")
               (null authenticated-method))
          "named auth bypasses selection and preserves the provider name")
         (setf authenticated-provider nil
               authenticated-method nil)
         (test-assert
          (eq
           (application--builtin-authentication-command application "chatgpt" "device")
           ':continue)
          "auth accepts an explicit ChatGPT authentication method")
         (test-assert
          (and (string= authenticated-provider "chatgpt")
               (string= authenticated-method "device"))
          "auth passes the explicit authentication method through unchanged")))))
  (let* ((configuration (test-configuration))
         (root (test-configuration-root configuration))
         (conversation (conversation-create configuration :identifier "callable-auth"))
         (terminal
          (make-instance 'stream-terminal :input-stream (make-string-input-stream "")
                         :output-stream (make-string-output-stream)
                         :input-file-descriptor -1 :columns 80))
         (application
          (make-instance 'application :configuration configuration :conversation
                         conversation :ui (terminal-ui-create :terminal terminal)))
         (picked-p nil)
         (authenticated-provider nil)
         (authenticated-method nil))
    (unwind-protect
        (test-call-with-function-replacements
         (list
          (list 'application--pick-authentication-provider
                (lambda (candidate)
                  (declare (ignore candidate))
                  (setf picked-p t)
                  "grok"))
          (list 'application-authenticate
                (lambda (candidate provider-name &optional method)
                  (declare (ignore candidate))
                  (setf authenticated-provider provider-name
                        authenticated-method method)
                  nil)))
         (lambda ()
           (test-assert (eq (application-run-lisp-input application "(auth)") ':continue)
            "callable auth with no provider completes through local Lisp")
           (test-assert (and picked-p (string= authenticated-provider "grok"))
            "callable auth with no provider opens provider selection")
           (setf picked-p nil
                 authenticated-provider nil
                 authenticated-method nil)
           (test-assert
            (eq (application-run-lisp-input application "(auth \"anthropic\")")
                ':continue)
            "callable auth with a provider completes through local Lisp")
           (test-assert
            (and (not picked-p) (string= authenticated-provider "anthropic")
                 (null authenticated-method))
            "callable auth with a provider bypasses selection")
           (setf authenticated-provider nil
                 authenticated-method nil)
           (test-assert
            (eq (application-run-lisp-input application "(auth \"chatgpt\" \"device\")")
                ':continue)
            "callable auth accepts an explicit authentication method")
           (test-assert
            (and (string= authenticated-provider "chatgpt")
                 (string= authenticated-method "device"))
            "callable auth passes its authentication method through")))
      (uiop/filesystem:delete-directory-tree root :validate t :if-does-not-exist
                                             ':ignore)))
  (let* ((terminal-output (make-string-output-stream))
         (captured-output (make-string-output-stream))
         (terminal
          (make-instance 'stream-terminal :input-stream (make-string-input-stream "")
                         :output-stream terminal-output :input-file-descriptor -1
                         :styled-p t :columns 80))
         (ui (terminal-ui-create :terminal terminal))
         (application (make-instance 'application :ui ui))
         (provider (make-instance 'application-authentication-test-provider))
         (stopped-p nil)
         (started-p nil))
    (test-call-with-function-replacements
     (list
      (list 'application--authentication-provider
            (lambda (candidate provider-name)
              (declare (ignore candidate))
              (test-assert (string= provider-name "grok")
               "auth forwards the selected provider name")
              provider))
      (list 'terminal-ui-stop
            (lambda (candidate) (declare (ignore candidate)) (setf stopped-p t) nil))
      (list 'terminal-ui-start
            (lambda (candidate) (declare (ignore candidate)) (setf started-p t) nil)))
     (lambda ()
       (let ((*standard-output* captured-output))
         (application-authenticate application "grok"))))
    (let ((terminal-text (get-output-stream-string terminal-output))
          (captured-text (get-output-stream-string captured-output)))
      (test-assert
       (and stopped-p started-p
            (eq (application-authentication-test-provider-stream provider)
                terminal-output)
            (application-authentication-test-provider-styled-p provider)
            (application-authentication-test-provider-open-browser-p provider)
            (= (application-authentication-test-provider-input-file-descriptor provider)
               -1))
       "auth uses the direct styled terminal and restores terminal ownership")
      (test-assert
       (and (search "Authenticating provider grok." terminal-text)
            (search "Open the provider login page now." terminal-text)
            (search "Provider authentication was saved." terminal-text))
       "auth immediately presents provider identity, instructions, and completion")
      (test-assert (not (search "Open the provider login page now." captured-text))
       "callable auth does not buffer login instructions in local Lisp output")))
  (let* ((terminal
          (make-instance 'application-authentication-test-localgroup-terminal
                         :direct-terminal nil :events
                         (list '(:paste "sek") ':backspace '(:insert "cret") ':submit)))
         (attachment-stream (make-string-output-stream))
         (controller
          (make-instance 'image-daemon:attachment :socket nil :stream attachment-stream
                         :mode ':control))
         (ui (terminal-ui-create :terminal terminal))
         (application (make-instance 'application :ui ui))
         (suspended-during-authentication-p nil)
         (repaint-during-authentication-p nil)
         (paste-input nil)
         (line-input nil)
         (provider
          (make-instance 'application-authentication-test-provider :read-input-p t
                         :activity-callback
                         (lambda ()
                           (setf suspended-during-authentication-p
                                   (terminal-ui-live-output-suspended-p ui))
                           (let ((history-length
                                  (length (image-daemon:relay-history-text terminal))))
                             (terminal-ui-stream-update ui :rows
                                                        (list
                                                         (list
                                                          (terminal-span ':plain
                                                                         "Deferred stream row.")))
                                                        :tail "Deferred stream tail.")
                             (terminal-ui-append-finalized ui ':deferred-auth
                                                           "Deferred finalized row.")
                             (terminal-ui--paint-live ui)
                             (setf repaint-during-authentication-p
                                     (/= history-length
                                         (length
                                          (image-daemon:relay-history-text
                                           terminal))))))))
         (stopped-p nil)
         (started-p nil))
    (setf (image-daemon:relay-controller terminal) controller
          (terminal-interactive-p terminal) t)
    (terminal-ui-start ui)
    (unwind-protect
        (progn
         (test-call-with-function-replacements
          (list
           (list 'application--authentication-provider
                 (lambda (candidate provider-name)
                   (declare (ignore candidate provider-name))
                   provider))
           (list 'terminal-ui-stop
                 (lambda (candidate)
                   (declare (ignore candidate))
                   (setf stopped-p t)
                   nil))
           (list 'terminal-ui-start
                 (lambda (candidate)
                   (declare (ignore candidate))
                   (setf started-p t)
                   nil)))
          (lambda ()
            (application-authenticate application "grok")
            (setf paste-input (application-authentication-test-provider-input provider)
                  (application-authentication-test-provider-input provider) nil
                  (application-authentication-test-localgroup-terminal-events terminal)
                    (list '(:line "line-secret")))
            (application-authenticate application "grok")
            (setf line-input (application-authentication-test-provider-input provider))))
         (let ((history (image-daemon:relay-history-text terminal)))
           (test-assert
            (and (not stopped-p) (not started-p) (terminal-ui-started-p ui)
                 (eq (image-daemon:relay-controller terminal) controller)
                 (not (application-authentication-test-provider-styled-p provider)))
            "localgroup auth preserves the attached plain terminal transport")
           (test-assert
            (and suspended-during-authentication-p (not repaint-during-authentication-p)
                 (not (terminal-ui-live-output-suspended-p ui))
                 (zerop (length (terminal-ui-deferred-live-appended-text ui)))
                 (zerop (length (terminal-ui-deferred-live-appended-display ui))))
            "localgroup auth defers concurrent live output until resume")
           (test-assert
            (and
             (typep (application-authentication-test-provider-stream provider)
                    'application-authentication-output-stream)
             (null
              (application-authentication-test-provider-input-file-descriptor provider))
             (string= paste-input "secret") (string= line-input "line-secret"))
            "localgroup auth accepts pasted, edited, and completed hidden lines")
           (test-assert
            (and (search "Authenticating provider grok." history)
                 (search "Open the provider login page now." history)
                 (search "Provider authentication was saved." history)
                 (search "Deferred stream row." history)
                 (search "Deferred finalized row." history))
            "localgroup auth shows direct and deferred output in order of ownership")))
      (terminal-ui-stop ui)))
  nil)

(-> test-application-mcp-reload-capability-change () null)
(defun test-application-mcp-reload-capability-change ()
  "Test /mcp reload reports only a changed deterministic tool-name set."
  (let ((application
          (make-instance 'application
                         :tool-registry (make-instance 'tool-registry)))
        (reload-count 0)
        (presented nil))
    (test-call-with-function-replacements
     (list
      (list
       'application-reload-mcp
       (lambda (candidate)
         (declare (ignore candidate))
         (if (= (incf reload-count) 1)
             (values '("alpha.add" "zeta.add") '("beta.remove"))
             (values nil nil))))
      (list
       'application-present
       (lambda (candidate entry)
         (test-assert (eq candidate application)
                      "/mcp presents capability changes through its application")
         (push entry presented)
         t)))
     (lambda ()
       (test-assert
        (eq (application--builtin-mcp-command application "reload") ':continue)
        "/mcp reload completes after reporting capability changes")
       (let* ((entries (nreverse presented))
              (change-entry (first entries)))
         (test-assert
          (and (= (length entries) 2)
               (listp change-entry)
               (string=
                (apply #'concatenate
                       'string
                       (mapcar #'terminal-span-text change-entry))
                "∙ tools added: alpha.add, zeta.add; removed: beta.remove")
               (equal (mapcar #'terminal-span-style change-entry)
                      '(:hint :success :hint :failure))
               (string= (second entries) "No MCP servers are configured."))
          "/mcp reload renders one deterministic added and removed tool chip"))
       (setf presented nil)
       (application--builtin-mcp-command application "reload")
       (test-assert
        (equal presented '("No MCP servers are configured."))
        "/mcp reload omits the capability chip when the tool-name set is unchanged")))
  nil))

(-> run-application-command-tests () boolean)
(defun run-application-command-tests ()
  "Run application command protocol tests and return true."
  (test-application-command-defining-form)
  (test-application-command-semantic-calls)
  (test-application-command-registry)
  (test-application-command-policies)
  (test-built-in-application-command-policies)
  (test-built-in-application-command-calls)
  (test-application-codex-fast-mode-command)
  (test-application-mcp-reload-capability-change)
  (test-terminal-authentication-streams)
  (test-application-authentication-command)
  t)
