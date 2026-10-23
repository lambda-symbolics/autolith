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
    (dolist (options '((t t) (nil nil) (t :on) (nil "off") (ordinary-symbol)))
      (test-assert
       (application-command-tests--macroexpand-error-p
        `(define-application-command application-command-tests--invalid-options
             (:name "/invalid-options"
              :description "invalid finite choices" :tip "tests finite option validation."
              :busy-behavior :inspect :terminal-behavior :shared
              :callable t :static-options ,options)
             (application value)
           (declare (ignore application value))
           :continue))
       "finite choices reject ambiguous spellings and non-literal symbols"))
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
  (application-command-tests--native-arguments)
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
                      :aliases '("/layered-command"))))
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
         ("/fix-skipped-definitions" :hold)
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
       '(("/help" (&optional topic))
         ("/resume" (&optional (identifier nil identifier-supplied-p)))
         ("/cwd" (&optional pathname))
         ("/trace" (&optional (mode ':status)))
         ("/timestamps" (&optional (mode ':status)))
         ("/ste" (&optional (mode ':status)))
         ("/titles" (&optional (mode ':status)))
         ("/hurry-up" (&optional (mode ':status)))
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
     (and (equal (application-command-static-options command) '(t nil))
          (string= (application-command-argument command) "[on|off]"))
     "native finite options derive their slash spelling from one metadata list"))
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
  (dolist (case '(("/mcp refresh typo" 3)
                  ("/permissions auto garbage" 3)))
    (destructuring-bind (input token-count) case
      (test-assert
       (= (length (application-command--tokens input)) token-count)
       (format nil "~A tokenizes every supplied slash argument" input))
      (test-assert
       (handler-case
           (let* ((invocation (application-command-invocation-parse input))
                  (command (application-command-invocation-command invocation)))
             (application-command-execute command nil invocation)
             nil)
         (configuration-error () t))
       (format nil "~A rejects excess slash arguments before handler effects" input))))
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
           (application--builtin-help-command application :extra :more)
           nil)
       (program-error ()
         t))
     "a built-in rejects arguments beyond its optional Lisp parameter")
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

(-> test-call-with-typed-control-application (function) null)
(defun test-call-with-typed-control-application (function)
  "Call FUNCTION with an isolated public-operation application and observed service actions."
  (multiple-value-bind (application root terminal tool)
      (application-operation-tests--application)
    (declare (ignore terminal tool))
    (let ((observations (list nil)))
      (unwind-protect
          (with-test-environment (("AUTOLITH_CODEX_FAST_MODE" nil))
            (test-call-with-function-replacements
             (list
              (list 'application-present
                    (lambda (candidate entry) (declare (ignore candidate entry)) t))
              (list 'application-publish-recovery-session
                    (lambda (candidate) (declare (ignore candidate)) nil))
              (list 'application-reset-history-pagination
                    (lambda (candidate) (declare (ignore candidate)) nil))
              (list 'application--install-configuration
                    (lambda (candidate replacement &key conversation)
                      (declare (ignore conversation))
                      (setf (application-configuration candidate) replacement)))
              (list 'slynk-start
                    (lambda (configuration)
                      (declare (ignore configuration))
                      (push ':start (first observations))))
              (list 'slynk-stop (lambda () (push ':stop (first observations))))
              (list 'slynk-status (lambda () (list :state ':stopped)))
              (list 'mcp-tool-registry-refresh
                    (lambda (registry)
                      (declare (ignore registry))
                      (push ':refresh (first observations))))
              (list 'application-reload-mcp
                    (lambda (candidate)
                      (declare (ignore candidate))
                      (push ':reload (first observations))
                      (values nil nil))))
             (lambda () (funcall function application root observations))))
        (tool-registry-close-runtime-state (application-tool-registry application))
        (platform-delete-directory-tree *platform* root :validate t :if-does-not-exist
                                        ':ignore))))
  nil)

(-> test-application-typed-boolean-controls () null)
(defun test-application-typed-boolean-controls ()
  "Test completed boolean calls, query defaults, slash parsing, and invalid controls."
  (test-call-with-typed-control-application
   (lambda (application root observations)
     (declare (ignore root observations))
     (let ((configuration (application-configuration application))
           (entries (application-operation-completion-entries application))
           (*read-eval* nil))
       (dolist
           (case
            '(("fast" :codex-fast-mode-p) ("trace" :reasoning-traces-p)
              ("timestamps" :turn-timestamps-p) ("cache-misses" :cache-miss-notices-p)
              ("ste" :simple-technical-english-p) ("titles" :session-title-generation-p)
              ("hurry-up" :hurry-up-p) ("compact-tool" :compact-view-p)))
         (destructuring-bind (name setting) case
           (let* ((completions
                    (remove-if-not
                     (lambda (entry) (equal (getf entry :primary) (format nil "(~A" name)))
                     entries))
                  (forms (mapcar (lambda (entry) (read-from-string (getf entry :name)))
                                 completions))
                  (boolean-completions
                    (remove-if-not
                     (lambda (entry)
                       (typep (second (read-from-string (getf entry :name))) 'boolean))
                     completions))
                  (hint (application-operation-completion-argument
                         (application-operation-find application name)))
                  (hint-values
                    (mapcar #'read-from-string
                            (uiop:split-string (string-trim '(#\[ #\] #\)) hint)
                                               :separator "|"))))
             (test-assert
              (equal (mapcar (lambda (entry) (second (read-from-string (getf entry :name))))
                             boolean-completions)
                     '(t nil))
              (format nil "~A completes both native boolean values" name))
             (test-assert (equal hint-values (mapcar #'second forms))
                          "native argument hints advertise the completed native values")
             (dolist (entry boolean-completions)
               (let* ((source (getf entry :name))
                      (value (second (read-from-string source)))
                      (evaluation (application-lisp-evaluate source :application application)))
                 (test-assert
                  (and (eq (application-lisp-evaluation-status evaluation) ':ok)
                       (eq (config setting configuration) value))
                  (format nil "completed ~A evaluates and applies its boolean value" source)))))
           (dolist (value '(t nil :on :off))
             (application-operation-call application name value)
             (test-assert
              (eq (config setting configuration) (not (null (member value '(t :on)))))
              (format nil "~A applies the native boolean ~S" name value)))
           (application-operation-call application name t)
           (let ((saved
                   (uiop/stream:read-file-string
                    (configuration-preferences-path configuration))))
             (application-operation-call application name)
             (application-operation-call application name ':status)
             (dolist (invalid '("on" 42 :unknown))
               (test-assert
                (handler-case
                    (progn (application-operation-call application name invalid) nil)
                  (configuration-error () t))
                (format nil "~A rejects non-native control ~S" name invalid)))
             (test-assert
              (and (eq (config setting configuration) t)
                   (string= saved
                            (uiop/stream:read-file-string
                             (configuration-preferences-path configuration))))
              "status and invalid controls do not change state or preferences"))
           (application-command application (format nil "/~A \"ON\"" name))
           (test-assert (config setting configuration) "slash on parses to a native action")
           (application-command application (format nil "/~A off" name))
           (test-assert (null (config setting configuration))
                        "slash off parses to a native action"))))))
  nil)

(-> test-application-codex-fast-mode-command () null)
(defun test-application-codex-fast-mode-command ()
  "Test /fast status, persistence, runtime installation, and validation."
  (test-application-typed-boolean-controls)
  (let* ((configuration
           (configuration-copy (test-configuration) :codex-fast-mode-p nil))
         (root (test-configuration-root configuration))
         (application (make-instance 'application
                                     :configuration configuration))
         (previous-fast-mode (uiop:getenv "AUTOLITH_CODEX_FAST_MODE"))
         (presented nil)
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
              'application--install-configuration
              (lambda (candidate replacement &key conversation)
                (declare (ignore conversation))
                (setf (application-configuration candidate) replacement)
                (push (config :codex-fast-mode-p replacement) installed)
                nil))
             (list
              'application-publish-recovery-session
              (lambda (candidate)
                (declare (ignore candidate))
                (incf published-count)
                nil)))
            (lambda ()
             (flet ((saved-fast-mode-p ()
                      "Return the Codex Fast mode value stored in the preferences file."
                      (getf (preferences--form->plist
                             (snapshot-read
                              (configuration-preferences-path
                               (application-configuration application))))
                            :codex-fast-mode-p)))
              (test-assert
               (eq (application--builtin-fast-command application) ':continue)
               "/fast status completes through the canonical command")
              (test-assert
               (search "Codex Fast mode is off." (first presented))
               "/fast reports the disabled state")
              (test-assert
               (eq (application--builtin-fast-command application ':status)
                   ':continue)
               "/fast status accepts its explicit spelling")
              (let ((codex-configuration
                      (application-configuration application)))
                (setf (application-configuration application)
                      (configuration-copy codex-configuration :model "grok-4.5"))
                (application--builtin-fast-command application ':status)
                (test-assert
                 (and (search "preference is off" (first presented))
                      (search "standard path" (first presented)))
                 "/fast identifies an inactive saved preference outside Codex")
                (setf (application-configuration application)
                      codex-configuration))
              (test-assert
               (and (not (probe-file (configuration-preferences-path
                                      (application-configuration application))))
                    (null installed)
                    (zerop published-count))
               "/fast status has no persistence or runtime side effects")
              (test-assert
               (eq (application--builtin-fast-command application t)
                   ':continue)
               "/fast on completes through the canonical command")
              (test-assert
               (and (config :codex-fast-mode-p
                     (application-configuration application))
                    (eq (saved-fast-mode-p) t)
                    (eq (first installed) t)
                    (= published-count 1)
                    (search "enabled and saved" (first presented)))
               "/fast on persists and installs the enabled state")
              (test-assert
               (eq (application--builtin-fast-command application nil)
                   ':continue)
               "/fast off completes through the canonical command")
              (test-assert
               (and (not
                     (config :codex-fast-mode-p
                      (application-configuration application)))
                    (null (saved-fast-mode-p))
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
              (application--builtin-fast-command application ':status)
              (test-assert
               (search "AUTOLITH_CODEX_FAST_MODE controls it" (first presented))
               "/fast status reports the process environment override")
              (test-assert
               (handler-case
                   (progn
                     (application--builtin-fast-command application t)
                     nil)
                 (configuration-error ()
                   t))
               "/fast cannot supersede the process environment override")
              (test-assert
               (and (null (saved-fast-mode-p))
                    (= (length installed) 2)
                    (= published-count 2))
               "invalid or environment-controlled /fast input has no side effects")))))
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
  (test-assert
   (handler-case
       (progn (application-command-invocation-parse "/auth grok device typo") nil)
     (authentication-error () t))
   "slash auth rejects excess arguments before command effects")
  (dolist (case '(("" nil)
                  ("\"MiXeD provider\"" ("MiXeD provider"))
                  ("chatgpt browser" ("chatgpt" :browser))
                  ("chatgpt device-code" ("chatgpt" :device-code))))
    (destructuring-bind (text expected) case
      (test-assert
       (equal (application-command-invocation-arguments
               (application-command-invocation-parse (concatenate 'string "/auth " text)))
              expected)
       "slash auth preserves identifiers and decodes only the method")))
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
           (application--builtin-authentication-command application "chatgpt" ':device-code)
           ':continue)
          "auth accepts an explicit ChatGPT authentication method")
         (test-assert
          (and (string= authenticated-provider "chatgpt")
               (eq authenticated-method ':device-code))
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
           (eq (application-run-lisp-input application "(auth \"chatgpt\" :device-code)")
                ':continue)
            "callable auth accepts an explicit authentication method")
           (test-assert
            (and (string= authenticated-provider "chatgpt")
                 (eq authenticated-method ':device-code))
            "callable auth passes its authentication method through")
           (test-assert
            (eq (application-command application "/auth chatgpt device")
                ':continue)
            "slash auth accepts textual device authentication")
           (test-assert (eq authenticated-method ':device-code)
            "slash auth decodes method text before calling the handler")))
      (uiop/filesystem:delete-directory-tree root :validate t :if-does-not-exist
                                             ':ignore)))
  (let ((application (make-instance 'application :ui nil))
        (effects nil))
    (test-call-with-function-replacements
     (list
      (list 'application--authentication-provider
            (lambda (candidate name)
              (declare (ignore candidate name))
              (make-instance 'codex-subscription-provider)))
      (list 'application-present
            (lambda (&rest arguments)
              (declare (ignore arguments))
              (push ':present effects)))
      (list 'application--authentication-streams
            (lambda (candidate)
              (declare (ignore candidate))
              (push ':streams effects))))
     (lambda ()
       (dolist (method '("device" :device :unknown 1))
         (test-assert
          (handler-case
              (progn (application-authenticate application "chatgpt" method) nil)
            (authentication-error () t))
          "native auth rejects an invalid method"))
       (test-assert (null effects)
        "invalid methods fail before presentation or terminal ownership"))))
  (let* ((terminal-output (make-string-output-stream))
         (captured-output (make-string-output-stream))
         (terminal
          (make-instance 'stream-terminal :input-stream (make-string-input-stream "")
                         :output-stream terminal-output :input-file-descriptor -1
                         :styled-p t :columns 80))
         (ui (terminal-ui-create :terminal terminal))
         (application (make-instance 'application :ui ui :configuration (make-configuration)))
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
         (application (make-instance 'application :ui ui :configuration (make-configuration)))
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

(-> test-application-typed-action-controls () null)
(defun test-application-typed-action-controls ()
  "Test keyword service and permission actions at public Lisp and slash boundaries."
  (test-call-with-typed-control-application
   (lambda (application root observations)
     (let ((configuration (application-configuration application)))
       (setf (config :slynk-directory configuration) root)
       (dolist
           (source
            '("(slynk :start)" "(slynk :status)" "(slynk :stop)" "(mcp :refresh)"
              "(mcp :reload)" "(mcp :status)"))
         (test-assert
          (eq
           (application-lisp-evaluation-status
            (application-lisp-evaluate source :application application))
           ':ok)
          "keyword service controls execute through local Lisp"))
       (test-assert (equal (reverse (first observations)) '(:start :stop :refresh :reload))
                    "status does not start, stop, refresh, or reload a service")
       (setf (first observations) nil)
       (dolist (source '("/slynk start" "/slynk stop" "/mcp refresh" "/mcp reload"))
         (application-command application source))
       (test-assert (equal (reverse (first observations)) '(:start :stop :refresh :reload))
                    "slash service actions use the same native handlers")
       (let ((before (first observations)))
         (dolist
             (call
              '((slynk "stop") (slynk :unknown) (mcp "refresh") (mcp nil) (permissions "full")
                (permissions nil)))
           (test-assert
            (handler-case
             (progn
              (apply #'application-operation-call application (first call) (rest call))
              nil)
             (configuration-error nil t))
            "invalid service and permission actions signal before effects"))
         (test-assert (eq (first observations) before)
                      "invalid service actions leave services untouched"))
       (application-operation-call application 'permissions ':ask)
       (test-assert (eq (config :permission-mode configuration) ':ask)
                    "keyword ask uses durable permission policy")
       (application-operation-call application 'permissions ':auto)
       (test-assert (eq (config :permission-mode configuration) ':auto)
                    "keyword auto uses durable permission policy")
       (application-operation-call application 'permissions ':full)
       (test-assert (eq (application-permission-mode application) ':full-access)
                    "keyword full uses session authority")
       (application-operation-call application 'permissions ':list)
       (application-operation-call application 'permissions ':clear))))
  nil)

(-> test-application-mcp-reload-capability-change () null)
(defun test-application-mcp-reload-capability-change ()
  "Test /mcp reload reports only a changed deterministic tool-name set."
  (test-application-typed-action-controls)
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
        (eq (application--builtin-mcp-command application ':reload) ':continue)
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
               (terminal-styled-text-p (second entries)))
          "/mcp reload renders one deterministic added and removed tool chip"))
       (setf presented nil)
       (application--builtin-mcp-command application ':reload)
       (test-assert
        (and (= (length presented) 1) (terminal-styled-text-p (first presented)))
        "/mcp reload omits the capability chip when the tool-name set is unchanged")))
  nil))

(-> test-application-typed-setting-values (application) null)
(defun test-application-typed-setting-values (application)
  "Check schema-defined lists, paths, literal strings, and case-sensitive choices."
  (let* ((*settings* (make-setting-registry))
         (original (application-configuration application)))
    (define-setting :typed-value (setting)
      :label "Typed value" :scope :session :default nil
      :type '(or null string list pathname))
    (define-setting :typed-choice (choice-setting)
      :label "Typed choice" :scope :session :default "Other"
      :type 'string :options '("Keep Case" "Other"))
    (let ((configuration (setinka:make-configuration)))
      (unwind-protect
           (progn
             (setf (application-configuration application) configuration)
             (dolist (value (list '("Keep Case" :keyword nil (1 2))
                                 #P"Mixed Case/file.lisp"
                                 "literal \" unmatched (text) \\ Keep Case"))
               (application-operation-call application 'settings ':typed-value value)
               (test-assert (eq (config :typed-value configuration) value)
                            "schema-accepted native values are not printed or reparsed"))
             (application-operation-call application 'settings ':typed-choice "Keep Case")
             (test-assert (string= (config :typed-choice configuration) "Keep Case")
                          "native string choice case is preserved")
             (test-assert
              (handler-case
                  (progn
                    (application-operation-call application 'settings ':typed-choice "keep case")
                    nil)
                (setting-invalid () t))
              "native choice validation does not silently normalize strings")
             (let ((literal "Keep \"quoted\" Case \\path"))
               (application-command
                application
                (format nil "/settings typed-value ~S" literal))
               (test-assert (string= (config :typed-value configuration) literal)
                            "slash quoted strings are decoded exactly once")))
        (setf (application-configuration application) original))))
  nil)

(-> test-application-typed-settings () null)
(defun test-application-typed-settings ()
  "Test native values and schema-aware slash settings without coercion of Lisp data."
  (test-call-with-typed-control-application
   (lambda (application root observations)
     (declare (ignore root observations))
     (let ((configuration (application-configuration application)))
       (application-operation-call application 'settings ':onboarding-p t)
       (let ((evaluation
              (application-lisp-evaluate
               "(progn (settings :onboarding-p nil) (settings :boot-screen-seconds 2))"
               :application application)))
         (test-assert
          (and (eq (application-lisp-evaluation-status evaluation) ':ok)
               (null (config :onboarding-p configuration))
               (= (config :boot-screen-seconds configuration) 2))
          "local Lisp settings accepts explicit NIL and numbers"))
       (application-operation-call application 'settings 'terminal-theme ':almighty)
       (test-assert (eq (config :terminal-theme configuration) ':almighty)
                    "symbol names and native choice values follow the schema")
       (application-command application "/settings boot-screen-seconds 3.5")
       (application-command application "/settings onboarding-p off")
       (test-assert
        (and (= (config :boot-screen-seconds configuration) 3.5)
             (null (config :onboarding-p configuration)))
        "slash settings parses values with the active setting schema")
       (let ((saved
              (uiop/stream:read-file-string (configuration-preferences-path configuration))))
         (dolist
             (arguments
              '((:boot-screen-seconds "2") (:onboarding-p "off") (:onboarding-p 2)
                (:terminal-theme :unknown)))
           (test-assert
            (handler-case
             (progn (apply #'application-operation-call application 'settings arguments) nil)
             (setting-invalid nil t))
            "invalid native settings signal the setting condition"))
         (test-assert
          (and (= (config :boot-screen-seconds configuration) 3.5)
               (eq (config :terminal-theme configuration) ':almighty)
               (string= saved
                        (uiop/stream:read-file-string
                         (configuration-preferences-path configuration))))
          "invalid settings neither mutate nor persist"))
       (test-application-typed-setting-values application))))
  nil)

(-> test-application-settings-command () null)
(defun test-application-settings-command ()
  "Test the settings page lists, shows, changes, and refuses settings."
  (test-application-typed-settings)
  (let* ((configuration (test-configuration))
         (root (test-configuration-root configuration))
         (application (make-instance 'application
                                     :configuration configuration))
         (presented nil)
         (picks nil))
    (unwind-protect
         (test-call-with-function-replacements
          (list
           (list 'application-present
                 (lambda (candidate entry)
                   (declare (ignore candidate))
                   (push entry presented)
                   t))
           (list 'application--pick-identifier
                 (lambda (candidate &rest arguments)
                   (declare (ignore candidate arguments))
                   (pop picks)))
           (list 'application-publish-recovery-session
                 (lambda (candidate)
                   (declare (ignore candidate))
                   nil)))
          (lambda ()
            (test-assert
             (eq (application--builtin-settings-command application) ':continue)
             "(settings) completes through the canonical command")
            (test-assert
             (and (search "cache-miss-notices-p = off (default)" (first presented))
                  (search "transcript" (first presented))
                  (not (search "provider-endpoint" (first presented))))
             "the listing shows visible settings with values and sources")
            (application--builtin-settings-command application "cache-miss-notices-p" t)
            (test-assert
             (and (application-cache-miss-notices-p application)
                  (search "Cache miss notices is now on and saved" (first presented)))
             "a named setting changes through the command")
            (test-assert
             (eq (configuration-setting-source configuration :cache-miss-notices-p) ':session)
             "an interactive change records the session source")
            (application--builtin-settings-command application "cache-miss-notices-p")
            (test-assert
             (search "Cache miss notices is on (this session)" (first presented))
             "a named setting without a value is described")
            (test-assert
             (handler-case
                 (progn
                   (application--builtin-settings-command application "immutable-p" t)
                   nil)
               (configuration-error (condition)
                 (not (null (search "cannot change" (format nil "~A" condition))))))
             "process settings are refused in a running session")
            (test-assert
             (handler-case
                 (progn
                   (application--builtin-settings-command application "no-such-setting" t)
                   nil)
               (setting-unknown () t))
             "unknown settings are refused")
            (test-assert
             (handler-case
                 (progn
                   (application--builtin-settings-command application "compaction-threshold-percent" "abc")
                   nil)
               (setting-invalid () t))
             "values are validated by the setting")
            (let ((*application-command-interactive-p* t))
              (setf picks (list "turn-timestamps-p" "on" "context-window" nil))
              (application--builtin-settings-command application)
              (test-assert
               (application-turn-timestamps-p application)
               "the page applies a picked option")
              (test-assert
               (and (search "Turn timestamps is now on" (second presented))
                    (search "read-only" (first presented)))
               "the page reports changes and read-only settings")
              (test-assert
               (null picks)
               "the page loops until the picker is cancelled"))))
      (platform-delete-directory-tree *platform* root :validate t :if-does-not-exist ':ignore)))


nil)

(-> test-authentication-preserves-non-copilot-model () null)
(defun test-authentication-preserves-non-copilot-model ()
  "Authenticate another registered account without changing the active backend."
  (with-test-environment (("AUTOLITH_MODEL" nil))
    (with-test-configuration (configuration)
      (setf (config :model configuration) *default-model*)
      (let ((snapshot (provider--registry-snapshot))
            (output (make-string-output-stream))
            (logins 0))
        (unwind-protect
             (progn
               (register-provider
                "authentication-other"
                :models '("authentication-other/test")
                :factory (lambda (selected &key reasoning-summaries-p)
                           (declare (ignore selected reasoning-summaries-p))
                           (make-instance 'model-provider))
                :authenticator (lambda (provider &key stream open-browser-p)
                                 (declare (ignore provider stream open-browser-p))
                                 (incf logins)
                                 "Provider authentication was saved."))
               (let ((*standard-output* output))
                 (main-authenticate configuration "authentication-other"))
               (test-assert (= logins 1) "startup authenticates the selected account")
               (test-assert (equal (config :model configuration) *default-model*)
                            "authenticating another account preserves the active model")
               (test-assert (equal (getf (preferences-load-values configuration) :model)
                                   *default-model*)
                            "startup persists the existing model selection"))
          (provider--registry-restore snapshot)))))
  nil)

(-> test-application-grouped-settings () null)
(defun test-application-grouped-settings ()
  "Test every visible setting is listed once, each group in one contiguous run."
  (with-test-configuration (configuration root)
    (declare (ignore root))
    (let* ((grouped (application--grouped-settings configuration))
           (runs (loop for (setting next) on grouped
                       when (or (null next)
                                (not (eq (setting-group setting) (setting-group next))))
                         collect (setting-group setting))))
      (test-assert (equal (sort (mapcar #'setting-name grouped) #'string<)
                          (sort (mapcar #'setting-name
                                        (remove-if-not #'setting-visible-p
                                                       (configuration-setting-list configuration)))
                                #'string<))
                   "every visible setting is listed once")
      (test-assert (= (length runs) (length (remove-duplicates runs)))
                   "each settings group forms one contiguous run")))
  nil)


;;;; -- Native Values and Slash Adapters --

(defvar *application-command-tests-parser-calls* nil
  "The complete remainders received by the slash parser fixture.")

(-> application-command-tests--slash-parser (string) list)
(defun application-command-tests--slash-parser (remainder)
  "Record REMAINDER and return it as one literal argument."
  (push remainder *application-command-tests-parser-calls*)
  (list remainder))

(defclass application-command-tests-unprintable () ()
  (:documentation "An opaque value whose printer must not run during invocation."))

(defmethod print-object ((object application-command-tests-unprintable) stream)
  "Fail if command presentation attempts to inspect OBJECT."
  (declare (ignore object stream))
  (error 'configuration-error :message "A command argument was printed."))

(-> application-command-tests--option-parser (string) list)
(defun application-command-tests--option-parser (text)
  "Parse the fixture's keyword action or literal string choice."
  (let ((values (application-command--tokens text)))
    (if (equal values '("on")) (list ':on) values)))

(-> application-command-tests--native-arguments () null)
(defun application-command-tests--native-arguments ()
  "Exercise native value transport, slash-only parsers and keyword binding."
  (dolist (parser '(nil t :parser "parser" (function parser)))
    (test-assert
     (application-command-tests--macroexpand-error-p
      `(define-application-command application-command-tests--invalid-parser
           (:name "/invalid-parser" :argument nil
            :description "invalid parser metadata" :tip "tests parser validation."
            :busy-behavior :inspect :terminal-behavior :shared
            :callable t :slash-parser ,parser)
           (application &rest arguments)
         (declare (ignore application arguments))
         :continue))
     "slash-parser metadata requires a literal non-NIL function-name symbol"))
  (let ((snapshot (application-command--registry-snapshot))
        (*application-command-tests-parser-calls* nil))
    (unwind-protect
         (progn
           (eval
            '(define-application-command application-command-tests--native
                 (:name "/native-values" :argument "[VALUES...]"
                  :description "test native command values"
                  :tip "tests native command values."
                  :busy-behavior :inspect :terminal-behavior :shared
                  :callable t
                  :slash-parser application-command-tests--slash-parser)
                 (application &rest values)
               (declare (ignore application))
               (setf *application-command-tests-semantic-call* values)
               :continue))
           (let* ((application (make-instance 'application))
                  (opaque (make-instance 'application-command-tests-unprintable))
                  (cycle (list :cycle))
                  (nested (list :objective "Keep Case" :criteria nil
                                (vector 1 (list :nested nil))))
                  (arguments (list 'ordinary-symbol nil 37 #P"relative/path"
                                   nested opaque cycle))
                  (command (application-command-find "/native-values")))
             (setf (rest cycle) cycle)
             (apply #'application-operation-call application "native-values" arguments)
             (test-assert
              (and (= (length arguments)
                      (length *application-command-tests-semantic-call*))
                   (every #'eq arguments *application-command-tests-semantic-call*))
              "symbols, NIL, numbers, paths, nested and opaque values retain identity")
             (test-assert (null *application-command-tests-parser-calls*)
                          "Lisp invocation never invokes a slash parser")
             (let* ((payload "quoted \"Mixed Case\" and an unmatched \"")
                    (invocation
                      (application-command-invocation-parse
                       (concatenate 'string "/native-values " payload))))
               (application-command-execute command application invocation)
               (test-assert
                (and (equal *application-command-tests-parser-calls* (list payload))
                     (equal *application-command-tests-semantic-call* (list payload)))
                "an explicit slash parser receives complete text before token parsing"))
             ;; A submitted invocation owns both handler and parser snapshots.
             (let ((old (application-command-invocation-parse "/native-values old"))
                   (registered (application-command--registry-snapshot)))
               (eval
                '(define-application-command application-command-tests--native
                     (:name "/native-values" :argument "[VALUES...]"
                      :description "replacement native command"
                      :tip "tests command replacement."
                      :busy-behavior :inspect :terminal-behavior :shared
                      :callable t)
                     (application &rest values)
                   (declare (ignore application))
                   (setf *application-command-tests-semantic-call*
                         (cons :replacement values))
                   :continue))
               (application-command-execute
                (application-command-invocation-command old) application old)
               (test-assert (equal *application-command-tests-semantic-call* '("old"))
                            "submitted invocations retain captured handler behavior")
               (let ((new (application-command-invocation-parse
                           "/native-values \"new value\"")))
                 (application-command-execute
                  (application-command-invocation-command new) application new)
                 (test-assert
                  (equal *application-command-tests-semantic-call*
                         '(:replacement "new value"))
                  "replacement without a parser uses ordinary slash tokenization"))
               (application-command--registry-restore registered)
               (let ((restored (application-command-invocation-parse
                                "/native-values restored \"literal\"")))
                 (application-command-execute
                  (application-command-invocation-command restored) application restored)
                 (test-assert
                  (equal *application-command-tests-semantic-call*
                         '("restored \"literal\""))
                  "restoring a registration restores its captured slash parser"))))
           (eval
            '(define-application-command application-command-tests--options
                 (:name "/native-options" :static-options (:on "MiXeD")
                  :description "native finite choices" :tip "tests typed option completion."
                  :busy-behavior :inspect :terminal-behavior :shared
                  :callable t :slash-parser application-command-tests--option-parser)
                 (application mode)
               (declare (ignore application))
               (setf *application-command-tests-semantic-call* (list mode))
               :continue))
           (let ((application (make-instance 'application))
                 (lisp-values nil)
                 (slash-values nil)
                 (*read-eval* nil))
             (dolist (entry (application-operation--command-option-completion-entries))
               (when (equal (getf entry :primary) "(native-options")
                 (let ((form (read-from-string (getf entry :name))))
                   (application-operation-call application "native-options" (second form))
                   (push (first *application-command-tests-semantic-call*) lisp-values))))
             (dolist (entry (application-command-option-completion-entries))
               (when (equal (getf entry :primary) "/native-options")
                 (application-command-execute
                  (application-command-find "/native-options") application
                  (application-command-invocation-parse (getf entry :name)))
                 (push (first *application-command-tests-semantic-call*) slash-values)))
             (test-assert (equal (reverse lisp-values) '(:on "MiXeD"))
                          "Lisp option completions retain keyword types and string case")
             (test-assert (equal slash-values lisp-values)
                          "slash and Lisp option completions invoke the same native choices"))
           (eval
            '(define-application-command application-command-tests--keywords
                 (:name "/native-keywords" :argument "[VALUE] [:PAYLOAD VALUE]"
                  :description "test keyword command values"
                  :tip "tests keyword command values."
                  :busy-behavior :inspect :terminal-behavior :shared :callable t)
                 (application &optional (value :default value-p)
                              &rest arguments &key (payload nil payload-p))
               (declare (ignore application))
               (setf *application-command-tests-semantic-call*
                     (list value value-p arguments payload payload-p))
               :continue))
           (let ((application (make-instance 'application)))
             (application-operation-call application "native-keywords")
             (test-assert
              (equal *application-command-tests-semantic-call* '(:default nil nil nil nil))
              "omitted optional and keyword values retain supplied-p semantics")
             (let ((payload (list :nested "literal")))
               (application-operation-call application "native-keywords" nil :payload payload)
               (test-assert
                (and (equal (subseq *application-command-tests-semantic-call* 0 2) '(nil t))
                     (eq (fourth *application-command-tests-semantic-call*) payload)
                     (fifth *application-command-tests-semantic-call*))
                "&REST &KEY preserves native values and explicit NIL supplied-p"))
             (dolist (arguments '((nil :unknown 1) (nil :payload)
                                  (nil ordinary-symbol 1)))
               (let ((before *application-command-tests-semantic-call*))
                 (test-assert
                  (handler-case
                      (progn
                        (apply #'application-operation-call
                               application "native-keywords" arguments)
                        nil)
                    (program-error () t))
                  "unknown or incomplete keyword arguments signal PROGRAM-ERROR")
                 (test-assert (eq before *application-command-tests-semantic-call*)
                              "invalid keyword invocation has no handler effects")))))
      (application-command--registry-restore snapshot)))
  nil)


;;;; -- Command View Semantics --

(-> command-view-tests--role-p (list keyword string) boolean)
(defun command-view-tests--role-p (entry style text)
  "Return true when ENTRY contains TEXT in the semantic STYLE."
  (not (null (find-if (lambda (span)
                        (and (eq (terminal-span-style span) style)
                             (search text (terminal-span-text span))))
                      entry))))

(-> test-command-view-fields-and-mcp () null)
(defun test-command-view-fields-and-mcp ()
  "Exercise cell-aligned labels and the severity of detached MCP server states."
  (let* ((fields (application--entry-fields '(("猫" :code "one") ("abc" :plain "two"))))
         (first-label (first fields))
         (second-label (fourth fields))
         (entry (application-mcp-status-entry
                 '((:name "ready-server" :state :ready :source :workspace
                    :transport :stdio :required-p nil :tool-count 4
                    :task-required-tool-count 0)
                   (:name "failed-server" :state :failed :source :global
                    :transport :http :required-p t :tool-count 0
                    :task-required-tool-count 2 :task-required-tool-reason "task execution"
                    :failure "connection refused")))))
    (test-assert (= (text-cell-width (terminal-span-text first-label))
                    (text-cell-width (terminal-span-text second-label)))
                 "field values align after labels with different character and cell widths")
    (test-assert (terminal-styled-text-p entry) "MCP status is a typed transcript entry")
    (dolist (role '((:success "ready") (:failure "failed") (:failure "connection refused")
                    (:notice "required") (:notice "task execution") (:dim "optional")))
      (test-assert (command-view-tests--role-p entry (first role) (second role))
                   "connection state, failure, policy and unavailable tools retain severity")))
  nil)

(-> test-command-view-generations () null)
(defun test-command-view-generations ()
  "Keep generation identity, compatibility, missing metadata and paths distinct."
  (with-test-configuration (configuration root)
    (let* ((replay (merge-pathnames "猫/reconstruct.lisp" root))
           (current (make-instance 'generation :identifier "current-generation"
                                              :metadata (list :git-commit "source-current"
                                                              :mutation-history-commit "history-current"
                                                              :reconstruction (namestring replay))))
           (legacy (make-instance 'generation :identifier "legacy-generation"
                                             :metadata '(:git-commit "source-legacy")))
           (application (make-instance 'application :configuration configuration))
           (entries nil))
      (test-call-with-function-replacements
       (list (list 'generation-list (lambda (configuration)
                                      (declare (ignore configuration))
                                      (list current legacy)))
             (list 'generation-compatible-p (lambda (generation) (eq generation current)))
             (list 'application-present (lambda (application entry)
                                           (declare (ignore application))
                                           (push entry entries) nil)))
       (lambda ()
         (application-operation-call application 'generations)
         (application-command application "/generations")
         (test-assert (= (length entries) 2) "native and slash generation commands both present")
         (test-assert (equal (first entries) (second entries)) "both forms use the same semantic view")
         (let ((entry (first entries)))
           (dolist (role (list '(:code "current-generation") '(:success "compatible")
                              '(:failure "incompatible") '(:notice "unavailable")
                              (list ':code (namestring replay))))
             (test-assert (command-view-tests--role-p entry (first role) (second role))
                          "generation states and reconstruction metadata have appropriate roles")))))))
  nil)

(-> test-command-view-model-warnings () null)
(defun test-command-view-model-warnings ()
  "Present discovered models and partial discovery failures through either command form."
  (with-test-configuration (configuration root)
    (declare (ignore root))
    (let* ((application (make-instance 'application :configuration configuration))
           (model (config :model configuration))
           (provider (provider-model-provider-name model))
           (failure (make-condition 'configuration-error :message "discovery unavailable"))
           (entries nil))
      (test-call-with-function-replacements
       (list (list 'provider-refresh-models (lambda (&rest options)
                                              (declare (ignore options)) (list failure)))
             (list 'application--credentialed-provider-names
                   (lambda (application) (declare (ignore application)) (list provider)))
             (list 'application-present (lambda (application entry)
                                           (declare (ignore application)) (push entry entries) nil)))
       (lambda ()
         (application-operation-call application 'models)
         (application-command application "/models")
         (test-assert (= (length entries) 2) "native and slash model discovery both present")
         (dolist (entry entries)
           (test-assert (terminal-styled-text-p entry) "model discovery returns semantic spans")
           (test-assert (command-view-tests--role-p entry ':code model) "model identifiers use code style")
           (test-assert (command-view-tests--role-p entry ':success "current") "current model is distinguished")
           (test-assert (command-view-tests--role-p entry ':notice "discovery unavailable")
                        "partial model discovery errors use warning severity"))))))
  nil)

(-> test-command-view-permissions-and-conversations () null)
(defun test-command-view-permissions-and-conversations ()
  "Present saved exact approvals and conversation metadata as semantic fields."
  (with-test-configuration (configuration root)
    (let* ((rules (list (make-instance 'command-permission :command "echo 猫"
                                                         :directory (namestring root))
                        (make-instance 'command-permission :command "server.lookup {}"
                                                           :kind ':external-tool
                                                           :directory (namestring root))))
           (application (make-instance 'application :configuration configuration
                                                    :permission-state (make-instance 'permission-state :rules rules)))
           (entries nil))
      (test-call-with-function-replacements
       (list (list 'application--conversation-items
                   (lambda (application)
                     (declare (ignore application))
                     '((:name "saved-conversation" :description "yesterday · workspace"))))
             (list 'application-present (lambda (application entry)
                                           (declare (ignore application)) (push entry entries) nil)))
       (lambda ()
         (application-operation-call application 'conversations)
         (application-command application "/conversations")
         (test-assert (every (lambda (entry)
                              (and (command-view-tests--role-p entry ':code "saved-conversation")
                                   (command-view-tests--role-p entry ':dim "yesterday"))) entries)
                      "conversation names and descriptive metadata have separate roles")
         (setf entries nil)
         (application-operation-call application 'permissions ':list)
         (application-command application "/permissions list")
         (test-assert (= (length entries) 2) "both command forms list saved approvals")
         (test-assert (every (lambda (entry)
                              (and (command-view-tests--role-p entry ':code "echo 猫")
                                   (command-view-tests--role-p entry ':code "server.lookup {}")
                                   (command-view-tests--role-p entry ':dim "external-tool"))) entries)
                      "saved shell and external requests retain code styling and domain metadata")
         (setf entries nil)
         (application-operation-call application 'permissions ':full)
         (test-assert (and (find ':notice (first entries) :key #'terminal-span-style)
                           (command-view-tests--role-p (first entries) ':code "full-access"))
                      "full-access receipt distinguishes mode and warning severity")
         (application-operation-call application 'permissions ':ask)
         (test-assert (and (find ':success (first entries) :key #'terminal-span-style)
                           (command-view-tests--role-p (first entries) ':code "ask"))
                      "ordinary permission changes distinguish mode and success severity")))))
  nil)
