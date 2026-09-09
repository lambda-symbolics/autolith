(in-package #:autolith)

;;;; -- User Operation Test Tool --

(defclass application-operation-test-tool (tool)
  ((calls
    :initform 0
    :accessor application-operation-test-tool-calls
    :type integer
    :documentation "The number of test executions observed.")
   (arguments
    :initform nil
    :accessor application-operation-test-tool-arguments
    :type t
    :documentation "The newest decoded argument object.")
   (context
    :initform nil
    :accessor application-operation-test-tool-context
    :type t
    :documentation "The newest local-user tool context."))
  (:documentation "A deterministic tool exercising the local operation boundary."))

(defmethod tool-execute
    ((tool application-operation-test-tool) (context tool-context) arguments)
  "Record one local operation execution and return its authorization decisions."
  (incf (application-operation-test-tool-calls tool))
  (setf (application-operation-test-tool-arguments tool) arguments
        (application-operation-test-tool-context tool) context)
  (tool-success
   (format nil
           "~(~A~) ~(~A~): ~A"
           (tool-context-authorize-command
            context "printf operation" (configuration-working-directory
                                         (tool-context-configuration context)))
           (tool-context-authorize-tool context tool arguments)
           (or (tool-argument arguments "text") "missing"))))


;;;; -- Operation Surface Tests --

(-> application-operation-tests--application
    ()
    (values application pathname recording-terminal application-operation-test-tool))
(defun application-operation-tests--application ()
  "Return one isolated application and its local operation test resources."
  (let* ((base-configuration (test-configuration))
         (root (test-configuration-root base-configuration))
         (configuration
           (configuration--clone base-configuration :working-directory root))
         (conversation
           (conversation-create configuration :identifier "operation-surface"))
         (registry (make-default-tool-registry))
         (tool
           (make-instance
            'application-operation-test-tool
            :namespace "test-operation"
            :name "echo"
            :description "Echo one local operation test value."
            :parameters
            (tool-object-schema
             (json-object
              "text" (tool-string-property "Text to echo.")
              "enabled" (json-object "type" "boolean")
              "items" (json-object "type" "array")
              "odd key)" (tool-string-property "Escaped completion key."))
             '("text"))))
         (terminal (make-instance 'recording-terminal :columns 100))
         (ui (terminal-ui-create :terminal terminal)))
    (tool-registry-register registry tool)
    (tool-registry-register
     registry
     (make-instance 'task-yield-tool
                    :namespace "yield"
                    :name "submit"
                    :description "Child-only test yield."
                    :parameters (tool-object-schema (json-object) nil)))
    (values
     (make-instance 'application
                    :configuration configuration
                    :conversation conversation
                    :provider nil
                    :tool-registry registry
                    :worker nil
                    :agent nil
                    :ui ui)
     root
     terminal
     tool)))

(-> test-prompt-operation () null)
(defun test-prompt-operation ()
  "Test local primary prompting, computed content, and named child steering."
  (let* ((configuration (test-configuration))
         (root (test-configuration-root configuration))
         (prompt-image (merge-pathnames "prompt-image.png" root))
         (conversation
           (conversation-create configuration :identifier "prompt-operation"))
         (registry (task-augment-tool-registry (make-default-tool-registry)))
         (primary
           (agent-create :configuration configuration
                         :conversation conversation
                         :tool-registry registry
                         :worker nil))
         (terminal (make-instance 'recording-terminal :columns 100))
         (ui (terminal-ui-create :terminal terminal))
         (application
           (make-instance 'application
                          :configuration configuration
                          :conversation conversation
                          :provider nil
                          :tool-registry registry
                          :worker nil
                          :agent primary
                          :ui ui))
         (controller nil)
         (orchestrator (application--task-orchestrator application))
         (definition
           (task-agent-definition-create
            :name "prompt-child"
            :description "Exercise named child prompting."
            :instructions "Accept ordered steering."
            :source ':test)))
    (labels ((mark-state (job state)
               (with-lock-held ((cl-jobpond::job--lock job))
                 (setf (job-state job) state))
               (task-job--set-progress-state job state)
               job)

             (prompt-reason (function)
               (handler-case
                   (progn
                     (funcall function)
                     nil)
                 (prompt-error (condition)
                   (prompt-error-reason condition))))

             (steering-items (job)
               (with-lock-held ((task-job-steering-lock job))
                 (deque->list (task-job-steering-items job))))

             (steering-texts (job)
               (mapcar
                (lambda (entry)
                  (user-message-input-text
                   (agent-steering-input-content entry)))
                (steering-items job))))
      (unwind-protect
           (progn
             (setf controller
                   (application-input-controller-create
                    application
                    :load-pending-p nil
                    :start-reader-p nil))
              (let ((path (merge-pathnames "prompt-content.txt" root)))
                (with-open-file (stream path
                                        :direction ':output
                                        :if-exists ':supersede
                                        :if-does-not-exist ':create
                                        :external-format ':utf-8)
                  (write-string "computed prompt text" stream))
                (test-conversation--write-tiny-png prompt-image)
                (let ((*application-operation-application* application))
                  (let ((receipt (prompt "ordinary prompt text")))
                    (test-assert
                     (and (eq (first receipt) ':prompt)
                          (getf (rest receipt) :accepted-p)
                          (eq (getf (rest receipt) :target) ':autolith)
                          (eq (getf (rest receipt) :delivery) ':queued))
                     "PROMPT returns a portable primary acceptance receipt"))
                  (let ((receipt
                          (prompt :to 'autolith "explicit primary prompt")))
                    (test-assert
                     (eq (getf (rest receipt) :target) ':autolith)
                     ":TO AUTOLITH explicitly targets the primary agent"))
                  (let ((receipt
                          (prompt :to "AuToLiTh"
                                  "case-insensitive primary prompt")))
                    (test-assert
                     (eq (getf (rest receipt) :target) ':autolith)
                     "string AUTOLITH targets the primary case-insensitively"))
                  (let ((receipt
                          (prompt :images (list prompt-image) "image prompt")))
                    (test-assert
                     (and (= (getf (rest receipt) :image-count) 1)
                          (= (getf (rest receipt) :content-characters)
                             (length "[Image #1] image prompt")))
                     "PROMPT returns primary image acceptance metadata")))
                (let ((evaluation
                        (application-lisp-evaluate
                         (format nil "(prompt (read-file ~S))" (namestring path))
                         :application application)))
                  (test-assert
                   (eq (application-lisp-evaluation-status evaluation) ':ok)
                   "computed READ-FILE prompt executes through local Lisp"))
                (let* ((work-items
                         (application-input-controller--state controller :work-items))
                       (image-input (second (fourth work-items))))
                  (test-assert
                   (equal
                    (mapcar
                     (lambda (work)
                       (user-message-input-text (second work)))
                     work-items)
                    '("ordinary prompt text"
                      "explicit primary prompt"
                      "case-insensitive primary prompt"
                      "[Image #1] image prompt"
                      "computed prompt text"))
                   "primary PROMPT preserves exact computed content and FIFO order")
                  (test-assert
                   (and (typep image-input 'user-message-input)
                        (equal (user-message-input-image-pathnames image-input)
                               (list (truename prompt-image))))
                   "primary PROMPT preserves validated image attachments"))
                (test-assert
                 (string= (read-file path) "computed prompt text")
                 "READ-FILE returns exact UTF-8 text")
                (test-assert
                 (eq (let ((*prompt-maximum-characters* 3))
                       (prompt-reason (lambda () (read-file path))))
                     ':content-too-large)
                 "READ-FILE enforces its bound while consuming one open stream"))
              (dolist (case
                       (list
                        (list nil ':malformed-arguments)
                        (list '("one" "two") ':malformed-arguments)
                        (list '(:to child) ':malformed-arguments)
                        (list '(:to 42 "text") ':invalid-target)
                        (list '(:images 42 "text") ':invalid-images)
                        (list '(:unknown t "text") ':malformed-arguments)
                        (list '("") ':empty-content)
                        (list '(42) ':invalid-content)))
                (destructuring-bind (arguments expected) case
                  (test-assert
                   (eq
                    (let ((*application-operation-application* application))
                      (prompt-reason
                       (lambda () (apply #'prompt arguments))))
                    expected)
                   (format nil "PROMPT rejects malformed arguments with ~S" expected))))
              (let ((circular-images (list prompt-image)))
                (setf (rest circular-images) circular-images)
                (test-assert
                 (eq (let ((*application-operation-application* application))
                       (prompt-reason
                        (lambda ()
                          (prompt :images circular-images "circular images"))))
                     ':invalid-images)
                 "PROMPT rejects circular image lists without traversing forever"))
              (test-assert
               (eq (let ((*application-operation-application* nil))
                     (prompt-reason (lambda () (prompt "text"))))
                   ':no-application)
               "PROMPT rejects calls outside local application evaluation")
              (test-assert
               (eq (let ((*application-operation-application* application)
                         (*prompt-maximum-characters* 3))
                     (prompt-reason (lambda () (prompt "four"))))
                   ':content-too-large)
               "PROMPT enforces its content bound before routing")
             (let* ((named
                      (task-tests--register-job
                       orchestrator primary definition
                       :name "shared-diff-final-review"))
                    (blocking
                      (task-tests--register-job
                       orchestrator primary definition
                       :name "blocking-review"
                       :detached-p nil))
                    (identifier-target
                      (task-tests--register-job
                       orchestrator primary definition
                       :name "identifier-review"))
                    (queued
                      (task-tests--register-job
                       orchestrator primary definition
                       :name "queued-review"))
                    (terminal-job
                      (task-tests--register-job
                       orchestrator primary definition
                       :name "terminal-review"))
                    (cancelled
                      (task-tests--register-job
                       orchestrator primary definition
                       :name "cancelled-review"))
                    (duplicate-one
                      (task-tests--register-job
                       orchestrator primary definition
                       :name "duplicate-review"))
                    (duplicate-two
                      (task-tests--register-job
                       orchestrator primary definition
                       :name "duplicate-review"))
                     (hidden
                       (task-tests--register-job
                        orchestrator primary definition
                        :name "hidden-review"
                        :owner-identifiers '("foreign-owner")
                        :root-conversation-identifier "foreign-root"))
                     (closing-job
                       (task-tests--register-job
                        orchestrator primary definition
                        :name "closing-review"))
                     (closed-job
                       (task-tests--register-job
                        orchestrator primary definition
                        :name "closed-review"))
                     (race-closing
                       (task-tests--register-job
                        orchestrator primary definition
                        :name "race-closing-review"))
                     (race-closed
                       (task-tests--register-job
                        orchestrator primary definition
                        :name "race-closed-review"))
                    (job-count
                      (length (task-orchestrator-list-jobs orchestrator))))
               (declare (ignore duplicate-one duplicate-two hidden))
                (mapc (lambda (job) (mark-state job ':running))
                      (list named blocking identifier-target closing-job closed-job
                            race-closing race-closed))
                (mark-state terminal-job ':completed)
                (task-job-cancel cancelled ':test-cancellation)
                (with-lock-held ((cl-jobpond::job--lock closing-job))
                  (setf (cl-jobpond::job--publication-claimed-p closing-job) t))
                (task-job-close-steering closed-job)
               (let ((*application-operation-application* application))
                 (let ((receipt
                         (prompt :to 'shared-diff-final-review
                                 "first child context")))
                   (test-assert
                    (and (eq (first receipt) ':prompt)
                         (getf (rest receipt) :accepted-p)
                         (eq (getf (rest receipt) :target) ':child)
                         (string= (getf (rest receipt) :child-name)
                                  "shared-diff-final-review")
                         (string= (getf (rest receipt) :job-id)
                                  (session-job-identifier named))
                         (string= (getf (rest receipt) :execution-id)
                                  (task-job-execution-identifier named))
                         (non-empty-string-p
                          (getf (rest receipt) :steering-id))
                         (eq (getf (rest receipt) :delivery) ':steering))
                    "named child PROMPT returns stable job and steering identity"))
                 (prompt :to "shared-diff-final-review" "second child context")
                 (test-assert
                  (equal (steering-texts named)
                         '("first child context" "second child context"))
                  "multiple child prompts enter the existing mailbox in FIFO order")
                  (let* ((receipt
                           (prompt :to 'shared-diff-final-review
                                   :images (list prompt-image)
                                   "image child context"))
                          (entry (first (last (steering-items named))))
                          (input (agent-steering-input-content entry)))
                    (test-assert
                     (and (eq (getf (rest receipt) :target) ':child)
                          (= (getf (rest receipt) :image-count) 1)
                          (typep input 'user-message-input)
                          (string= (user-message-input-text input)
                                   "[Image #1] image child context")
                          (equal (user-message-input-image-pathnames input)
                                 (list (truename prompt-image))))
                     "child PROMPT preserves validated image attachments"))
                  (let ((*task-steering-maximum-items* 2))
                    (test-assert
                     (eq (prompt-reason
                          (lambda ()
                            (prompt :to "shared-diff-final-review" "overflow")))
                         ':full)
                     "PROMPT reports a full child steering mailbox"))
                  (let ((*task-steering-maximum-characters* 3))
                    (test-assert
                     (eq (prompt-reason
                          (lambda ()
                            (prompt :to "shared-diff-final-review" "four")))
                         ':content-too-large)
                     "PROMPT reports the child steering content bound"))
                 (let ((receipt
                         (prompt :to "blocking-review" "blocking context")))
                   (test-assert
                    (and (eq (first receipt) ':prompt)
                         (getf (rest receipt) :accepted-p)
                         (not (task-job-detached-p blocking)))
                    "blocking children accept steering without respawn"))
                 (let ((receipt
                         (prompt :to (session-job-identifier identifier-target)
                                 "identifier context")))
                   (test-assert
                    (string= (getf (rest receipt) :job-id)
                             (session-job-identifier identifier-target))
                    "PROMPT falls back from display name to visible job ID"))
                  (dolist (case
                           (list
                            (list "missing-review" ':unknown-target)
                            (list "duplicate-review" ':ambiguous-target)
                            (list "queued-review" ':not-running)
                            (list "terminal-review" ':terminal)
                            (list "cancelled-review" ':cancelled)
                            (list "closing-review" ':closing)
                            (list "closed-review" ':closed)
                            (list "hidden-review" ':unknown-target)))
                    (destructuring-bind (target expected) case
                      (test-assert
                       (eq (prompt-reason
                            (lambda () (prompt :to target "context")))
                           expected)
                       (format nil "child target ~A rejects with ~S"
                               target expected)))))
                (let ((*application-operation-application* application)
                      (original-enqueue
                        (symbol-function 'task-job-enqueue-steering)))
                  (test-call-with-function-replacements
                   (list
                    (list
                     'task-job-enqueue-steering
                     (lambda (job content &key (promote-response-p nil))
                       (cond
                         ((eq job race-closing)
                          (with-lock-held ((cl-jobpond::job--lock job))
                            (setf (cl-jobpond::job--publication-claimed-p job) t)))
                         ((eq job race-closed)
                          (task-job-close-steering job)))
                       (funcall original-enqueue
                                job content
                                :promote-response-p promote-response-p))))
                   (lambda ()
                     (test-assert
                      (eq (prompt-reason
                           (lambda ()
                             (prompt :to "race-closing-review" "context")))
                          ':closing)
                      "PROMPT reports closure beginning after child lookup")
                     (test-assert
                      (eq (prompt-reason
                           (lambda ()
                             (prompt :to "race-closed-review" "context")))
                          ':closed)
                      "PROMPT reports mailbox closure after child lookup")))
                  (test-assert
                   (and (zerop (task-job-steering-pending-count race-closing))
                        (zerop (task-job-steering-pending-count race-closed)))
                   "racing child closure never admits a steering entry"))
               (test-assert
                (= (length (task-orchestrator-list-jobs orchestrator)) job-count)
                "named child prompting never respawns or changes job identity")))
        (when controller
          (application-input-controller-stop controller))
        (ignore-errors (terminal-ui-stop ui))
        (ignore-errors (tool-registry-close-runtime-state registry))
        (uiop:delete-directory-tree root :validate t
                                         :if-does-not-exist ':ignore))))
  nil)

(-> test-tool-boolean-argument () null)
(defun test-tool-boolean-argument ()
  "Test optional JSON Boolean arguments preserving explicit false values."
  (test-assert
   (null (tool-boolean-argument (json-object "async" nil) "async"))
   "an explicit NIL JSON false is accepted")
  (test-assert
   (null (tool-boolean-argument (json-object "async" false) "async"))
   "an explicit Yason JSON false is accepted")
  (test-assert
   (tool-boolean-argument (json-object "async" t) "async")
   "an explicit JSON true is accepted")
  (test-assert
   (tool-boolean-argument (json-object) "async" :default t)
   "an absent Boolean uses its true default")
  (test-assert
   (handler-case
       (progn
         (tool-boolean-argument (json-object "async" "false") "async")
         nil)
     (tool-error ()
       t))
   "a non-Boolean value is rejected")
  nil)

(-> test-application-operation-binding-conflicts () null)
(defun test-application-operation-binding-conflicts ()
  "Test canonical operation conflicts fail registration and never break evaluation."
  (let* ((command
           (application-command-create
            :definition-name 'application-operation-tests--conflicting-search
            :name "/search"
            :aliases nil
            :argument nil
            :description "Conflict with the inherited Common Lisp SEARCH function."
            :tip "exists for binding conflict tests."
            :busy-behavior ':inspect
            :terminal-behavior ':shared
            :lambda-list nil
            :callable-p nil
            :static-options nil
            :handler
            (lambda (application invocation)
              (declare (ignore application invocation))
              ':continue)))
         (operation (application-operation--from-command command))
         (symbol (application-operation--function-symbol "search"))
         (existing (fdefinition symbol))
         (snapshot (application-command--registry-snapshot)))
    (test-assert
     (handler-case
         (progn
           (register-application-command command :source ':test)
           nil)
       (configuration-error ()
         t))
     "command registration rejects canonical Lisp function conflicts")
    (test-assert
     (equal snapshot (application-command--registry-snapshot))
     "rejected canonical binding conflicts leave the command registry unchanged")
    (test-assert
     (null (application-operation--install-binding operation))
     "binding installation skips conflicting legacy operations")
    (test-assert
     (eq (fdefinition symbol) existing)
     "skipped operation bindings preserve the existing function"))
  nil)

(-> run-application-operation-tests () boolean)
(defun run-application-operation-tests ()
  "Run focused unified command and tool operation tests."
  (test-prompt-operation)
  (test-tool-boolean-argument)
  (test-application-operation-binding-conflicts)
  (multiple-value-bind (application root terminal tool)
      (application-operation-tests--application)
    (unwind-protect
         (let* ((operations (application-operation-list application))
                (names (mapcar #'application-operation-name operations))
                (command-operation
                  (application-operation-find application 'help))
                (local-operation
                  (application-operation-find application 'eval-now))
                (tool-operation
                  (application-operation-find application "RESOURCE.READ")))
           (test-assert (member "help" names :test #'string=)
                        "interactive commands appear as canonical Lisp operations")
           (test-assert (member "update" names :test #'string=)
                        "the attended release update is a canonical Lisp operation")
           (test-assert (member "resource.read" names :test #'string=)
                        "model tools appear in the same user operation registry")
           (test-assert (member "lisp.paren-check" names :test #'string=)
                        "the built-in source checker is a canonical Lisp operation")
           (test-assert (member "test-operation.echo" names :test #'string=)
                        "per-session tools appear in the local operation registry")
           (test-assert (member "eval-now" names :test #'string=)
                        "the immediate local evaluator is a discoverable operation")
           (test-assert (member "prompt" names :test #'string=)
                        "the canonical primary and child prompt is discoverable")
           (test-assert (member "read-file" names :test #'string=)
                        "bounded prompt file input is discoverable")
           (test-assert
            (and (typep command-operation 'application-command-operation)
                 (typep (application-operation-backend command-operation)
                        'application-command)
                 (typep local-operation 'application-local-operation)
                 (symbolp (application-operation-backend local-operation))
                 (typep tool-operation 'application-tool-operation)
                 (typep (application-operation-backend tool-operation) 'tool))
            "operation subclasses expose their typed backend protocol")
           (test-assert
            (and (application-operation-binding-eligible-p command-operation)
                 (not (application-operation-binding-eligible-p local-operation))
                 (application-operation-binding-eligible-p tool-operation))
            "operation subclasses control canonical binding eligibility")
           (test-assert
            (every (lambda (name)
                     (typep (fdefinition name) 'generic-function))
                   '(application-operation-kind
                     application-operation-completion-argument
                     application-operation-active-turn-action
                     application-operation-immediate-arguments-p
                     application-operation-invoke
                     application-operation-binding-eligible-p))
            "operation behavior is represented by CLOS generic functions")
           (test-assert (eq (application-operation-kind local-operation) ':local)
                        "operation lookup identifies the immediate local form")
           (test-assert (not (member "yield.submit" names :test #'string=))
                        "the child-only yield operation stays hidden from users")
           (test-assert (eq (application-operation-kind command-operation) ':command)
                        "operation lookup resolves command symbols case-insensitively")
           (test-assert (eq (application-operation-kind tool-operation) ':tool)
                        "operation lookup resolves dotted tool names case-insensitively")
           (let* ((provider (terminal-ui-completion-function
                             (application-ui application)))
                  (entries (and provider (funcall provider)))
                  (entry-names (mapcar (lambda (entry) (getf entry :name))
                                       entries))
                  (resource-entry
                    (find-if
                     (lambda (entry)
                       (string= (getf entry :name) "(resource.read"))
                     entries))
                  (paren-entry
                    (find-if
                     (lambda (entry)
                       (string= (getf entry :name) "(lisp.paren-check"))
                     entries))
                  (test-entry (find-if
                               (lambda (entry)
                                 (string= (getf entry :name)
                                          "(test-operation.echo"))
                               entries)))
             (test-assert (functionp provider)
                          "applications install a dynamic operation completion provider")
             (test-assert (member "/help" entry-names :test #'string=)
                          "slash compatibility completion retains canonical commands")
             (test-assert (member "(help)" entry-names :test #'string=)
                          "completion offers a canonical no-argument Lisp command")
              (test-assert
               (and (member "/ste on" entry-names :test #'string=)
                    (member "/ste off" entry-names :test #'string=))
               "slash completion offers every finite command option")
              (test-assert
               (and (member "(ste \"on\")" entry-names :test #'string=)
                    (member "(ste \"off\")" entry-names :test #'string=))
               "Lisp completion offers every finite command option")
              (flet ((primary-of (name)
                       "Return the :PRIMARY of the completion entry named NAME."
                       (getf (find name entries
                                   :key (lambda (entry) (getf entry :name))
                                   :test #'string=)
                             :primary)))
                (test-assert
                 (and (equal (primary-of "/ste on") "/ste")
                      (equal (primary-of "(ste \"off\")") "(ste")
                      (null (primary-of "/ste"))
                      (null (primary-of "(help)")))
                 "finite option entries stay behind their canonical command rows"))
             (test-assert (member "(update)" entry-names :test #'string=)
                          "completion offers the explicit release update operation")
             (test-assert (member "(eval-now" entry-names :test #'string=)
                          "completion offers the explicit immediate local form")
             (test-assert (member "(prompt" entry-names :test #'string=)
                          "completion offers the canonical prompt operation")
             (test-assert (member "(read-file" entry-names :test #'string=)
                          "completion offers bounded computed prompt input")
             (test-assert
              (and resource-entry
                   (search ":uri" (or (getf resource-entry :argument) "")))
              "completion exposes dotted tool names with Lisp keyword arguments")
             (test-assert
              (and paren-entry
                   (search ":path" (or (getf paren-entry :argument) "")))
              "completion exposes the built-in source check path")
             (test-assert
              (and test-entry
                   (search ":|odd key)| VALUE"
                           (or (getf test-entry :argument) "")))
              "completion escapes punctuation and whitespace in property names")
             (test-assert
              (not (member "/resource.read" entry-names :test #'string=))
              "slash compatibility does not invent tool spellings")
             (test-assert
              (not (find-if (lambda (name)
                              (uiop:string-prefix-p "(yield.submit" name))
                            entry-names))
              "completion hides child-only operations"))
           (let ((unclassified
                   (make-instance
                    'tool
                    :namespace "unclassified"
                    :name "safe-looking"
                    :description "An intentionally unclassified test tool."
                    :parameters (tool-object-schema (json-object) nil))))
             (test-assert (eq (tool-active-turn-action unclassified) ':hold)
                          "unclassified tools wait regardless of their names"))
           (dolist (case
                     '(("(help)" :execute)
                       ("(prompt \"text\")" :execute)
                       ("(prompt (read-file \"/tmp/example\"))" :execute)
                        ("(goal \"pause\")" :hold)
                       ("(quit)" :cancel)
                       ("(update)" :hold)
                        ("(compact)" :hold)
                       ("(resource.read :uri \"workspace:.\")" :execute)
                       ("(lisp.paren-check :path \".\")" :execute)
                       ("(shell.run :command \"true\")" :hold)
                       ("(test-operation.echo :text \"hello\")" :hold)
                       ("(self.status)" :execute)
                       ("(self.eval :form \"(+ 1 2)\")" :hold)
                       ("(resource.read :uri (progn (setf *print-base* 8) \"workspace:.\"))"
                        :hold)
                       ("(eval-now (setf *print-base* 8))" :execute)))
             (destructuring-bind (source expected) case
               (test-assert
                (eq (application-operation-source-active-turn-action
                     application source)
                    expected)
                (format nil "~A has active-turn action ~S" source expected))))
           (let ((evaluation
                   (application-lisp-evaluate
                    "(eval-now :not-local-input)"
                    :application application)))
             (test-assert
              (and (eq (application-lisp-evaluation-status evaluation) ':aborted)
                   (search "explicit local Lisp input"
                           (or (application-lisp-evaluation-condition evaluation) "")))
              "eval-now rejects noninteractive evaluator callers"))
           (let ((path (merge-pathnames "operation-check.lisp" root)))
             (with-open-file (stream path
                                     :direction ':output
                                     :if-exists ':supersede
                                     :if-does-not-exist ':create
                                     :external-format ':utf-8)
               (write-string "(defun operation-check () 42)" stream))
             (let ((evaluation
                     (application-lisp-evaluate
                      (format nil "(lisp.paren-check :path ~S)"
                              (namestring path))
                      :application application)))
               (test-assert
                (and (eq (application-lisp-evaluation-status evaluation) ':ok)
                     (some (lambda (value)
                             (search "No unmatched or mismatched delimiters found"
                                     value))
                           (application-lisp-evaluation-values evaluation)))
                "canonical Lisp dispatch executes the built-in source checker")))
           (let ((command-authorizations 0)
                 (tool-authorizations 0))
             (test-call-with-function-replacements
              (list
               (list 'application-authorize-command
                     (lambda (observed command directory)
                       (test-assert (eq observed application)
                                    "local shell authority stays with the application")
                       (test-assert (string= command "printf operation")
                                    "local tools preserve exact shell authorization text")
                       (test-assert
                        (equal directory
                               (configuration-working-directory
                                (application-configuration application)))
                        "local tools preserve the authorized working directory")
                       (incf command-authorizations)
                       ':sandboxed))
               (list 'application-authorize-tool
                     (lambda (observed observed-tool arguments)
                       (test-assert (eq observed application)
                                    "local external-tool authority stays with the application")
                       (test-assert (eq observed-tool tool)
                                    "local authorization receives the authoritative tool object")
                       (test-assert (json-object-p arguments)
                                    "local authorization receives decoded JSON arguments")
                       (incf tool-authorizations)
                       ':allow)))
              (lambda ()
                (let ((evaluation
                        (application-lisp-evaluate
                         "(test-operation.echo :text \"hello\" :enabled nil :items '(1 :two))"
                         :application application)))
                  (test-assert
                   (and (eq (application-lisp-evaluation-status evaluation) ':ok)
                        (equal (application-lisp-evaluation-values evaluation)
                               '("\"sandboxed allow: hello\"")))
                   "a typed tool form executes through the ordinary decoder and method"))))
             (test-assert (= (application-operation-test-tool-calls tool) 1)
                          "a local tool form executes its tool object exactly once")
             (test-assert (= command-authorizations 1)
                          "a local tool reuses shell authorization exactly once")
             (test-assert (= tool-authorizations 1)
                          "a local tool reuses external authorization exactly once"))
           (let* ((arguments (application-operation-test-tool-arguments tool))
                  (items (tool-argument arguments "items")))
             (test-assert (null (tool-argument arguments "enabled"))
                          "Lisp NIL crosses the local tool boundary as JSON false")
             (test-assert
              (and (vectorp items)
                   (= (length items) 2)
                   (= (aref items 0) 1)
                   (string= (aref items 1) "two"))
              "local Lisp sequences and keyword enum values cross as JSON values"))
           (let ((context (application-operation-test-tool-context tool)))
             (test-assert
              (and (typep context 'tool-context)
                   (eq (tool-context-conversation context)
                       (application-conversation application))
                   (eq (tool-context-registry context)
                       (application-tool-registry application))
                   (null (tool-context-agent context)))
              "local operation execution receives the primary application context"))
           (application-operation-install-bindings application)
           (dolist (name '(trace papercut-close resource.read test-operation.echo))
             (test-assert (fboundp name)
                          (format nil "canonical operation ~S has a Lisp function binding"
                                  name)))
            (setf (application-goal application)
                  (list :objective "existing"
                        :status ':paused
                        :continuations 0
                        :created-at (get-universal-time)))
            (let ((evaluation
                    (application-lisp-evaluate
                     "(goal 'clear)"
                     :application application)))
              (test-assert
               (and (eq (application-lisp-evaluation-status evaluation) ':ok)
                    (null (application-goal application)))
               "raw-remainder Lisp commands normalize evaluated values to text"))
           (recording-terminal-reset terminal)
           (let* ((evaluation
                    (application-lisp-evaluate
                     "(progn (help) :finished)"
                     :application application))
                  (output (recording-terminal-output terminal)))
             (test-assert
              (and (eq (application-lisp-evaluation-status evaluation) ':ok)
                   (equal (application-lisp-evaluation-values evaluation)
                          '(":FINISHED"))
                   (search "(help)" output)
                   (search "(resource.read" output)
                   (search "Slash commands remain compatibility spellings."
                           output))
              "nested help shows canonical command and tool operations"))
           (recording-terminal-reset terminal)
           (test-assert (eq (application--run-command-input application "/help")
                            ':continue)
                        "slash compatibility still executes the command backend")
           (test-assert
            (search "Prefer (help)." (recording-terminal-output terminal))
            "the first slash spelling shows its canonical Lisp form")
           (recording-terminal-reset terminal)
           (application--run-command-input application "/help")
           (test-assert
            (not (search "Prefer (help)." (recording-terminal-output terminal)))
            "a command's preferred Lisp spelling appears only once per session")
           (recording-terminal-reset terminal)
           (test-assert
            (and (eq (application--run-command-input application "/exit") ':quit)
                 (search "Prefer (quit)." (recording-terminal-output terminal)))
            "slash aliases hint the canonical command operation name")
           (let ((evaluation
                   (application-lisp-evaluate "(quit)" :application application)))
             (test-assert
              (and (eq (application-lisp-evaluation-status evaluation) ':ok)
                   (eq (application-lisp-evaluation-loop-action evaluation) ':quit)
                   (null (application-lisp-evaluation-values evaluation)))
              "a canonical quit operation transfers control to the application loop"))
           (let ((*update-check-fetch-function* (lambda () "v99.0.0")))
             (setf (application-installation-provenance application)
                   (make-instance
                    'installation-provenance
                    :method ':release
                    :current-tag (format nil "v~A" *autolith-version*)))
             (test-assert
              (handler-case
                  (progn
                    (application-lisp-evaluate
                     "(update)" :application application)
                    nil)
                (update-requested (condition)
                  (string= (update-requested-tag condition) "v99.0.0")))
              "a canonical update operation requests the packaged launcher handoff")
             (test-assert
              (handler-case
                  (progn
                    (application--run-command-input application "/update")
                    nil)
                (update-requested (condition)
                  (string= (update-requested-tag condition) "v99.0.0")))
              "the slash update spelling requests the same launcher handoff"))
           (let ((evaluation
                   (application-lisp-evaluate
                    "(test-operation.echo :text)"
                    :application application)))
             (test-assert
              (and (eq (application-lisp-evaluation-status evaluation) ':aborted)
                   (search "alternating keyword and value"
                           (or (application-lisp-evaluation-condition evaluation) "")))
              "malformed local tool argument plists fail through the Lisp condition boundary"))
           t)
      (ignore-errors (terminal-ui-stop (application-ui application)))
      (uiop:delete-directory-tree root :validate t :if-does-not-exist ':ignore))))


;;;; -- On-demand Compaction --

(-> application-operation-tests--call-with-compaction-provider
    (model-provider function) null)
(defun application-operation-tests--call-with-compaction-provider (provider function)
  "Call FUNCTION with an isolated application using PROVIDER for compaction."
  (multiple-value-bind (application root terminal)
      (application-operation-tests--application)
    (declare (ignore terminal))
    (unwind-protect
         (progn
           (setf (application-provider application) provider
                 (application-agent application)
                 (agent-create
                  :configuration (application-configuration application)
                  :conversation (application-conversation application)
                  :provider provider
                  :tool-registry (application-tool-registry application)
                  :worker nil))
           (funcall function application))
      (terminal-ui-stop (application-ui application))
      (uiop:delete-directory-tree root :validate t :if-does-not-exist ':ignore)))
  nil)

(-> test-compact-operation () null)
(defun test-compact-operation ()
  "Test forced portable and native compaction through the canonical Lisp command."
  (dolist (native-p '(nil t))
    (let* ((native-item
             (json-object "type" "context_compaction"
                          "encrypted_content" "command-checkpoint"))
           (provider
             (apply #'make-instance
                    (if native-p 'native-scripted-provider 'scripted-provider)
                    :results (list (agent-test-result
                                    "manual-summary"
                                    (list (agent-test-message "A portable handoff."))))
                    (when native-p (list :native-items (list native-item))))))
      (application-operation-tests--call-with-compaction-provider
       provider
       (lambda (application)
         (let* ((conversation (application-conversation application))
                (configuration (application-configuration application)))
           (conversation-append-user-message conversation "Earlier work.")
           (let* ((sequence-before (conversation-next-sequence conversation))
                  (evaluation (application-lisp-evaluate
                               "(compact)" :application application))
                  (records-after (conversation--read-records
                                 (conversation-pathname conversation)))
                  (reloaded (conversation-load-by-id
                             configuration (conversation-identifier conversation))))
             (test-assert (eq (application-lisp-evaluation-status evaluation) ':ok)
                          "the registered Lisp command compacts below the automatic threshold")
             (test-assert
              (and (equal (scripted-provider-compaction-flags provider) '(t))
                   (equal (scripted-provider-tool-schema-counts provider) '(0)))
              "manual compaction makes one tool-free summary request")
             (test-assert
              (= (conversation-next-sequence conversation) (1+ sequence-before))
              "manual compaction appends one checkpoint without starting a user turn")
             (test-assert
              (eq (first (first (last records-after)))
                  (if native-p ':native-compaction ':summary))
              "the command uses the provider's existing compaction strategy")
             (test-assert
              (string= (json-encode (coerce (conversation-input-items conversation) 'vector))
                       (json-encode (coerce (conversation-input-items reloaded) 'vector)))
              "the compacted projection is reproducible from durable history")
             (test-assert
              (and (not (terminal-ui-compacting-p (application-ui application)))
                   (null (terminal-ui-status (application-ui application))))
              "manual compaction clears its progress indicators")))))))
  nil)

(-> test-compact-operation-empty () null)
(defun test-compact-operation-empty ()
  "Test empty conversations need no connected agent or durable checkpoint."
  (let ((provider (make-instance 'scripted-provider :results nil)))
    (application-operation-tests--call-with-compaction-provider
     provider
     (lambda (application)
       (setf (application-agent application) nil)
       (let* ((conversation (application-conversation application))
              (evaluation (application-lisp-evaluate
                           "(compact)" :application application)))
         (test-assert (eq (application-lisp-evaluation-status evaluation) ':ok)
                      "compacting an empty conversation succeeds without an agent")
         (test-assert
          (and (null (scripted-provider-input-snapshots provider))
               (null (conversation-input-items conversation))
               (not (conversation-persisted-p conversation)))
          "empty compaction neither contacts the provider nor creates history")
         (conversation-append-user-message conversation "Needs a connected agent.")
         (test-assert
          (handler-case
              (progn (application-command application "/compact") nil)
            (configuration-error ()
              t))
          "nonempty compaction without an agent reports a configuration failure")))))
  nil)

(-> test-compact-operation-failures () null)
(defun test-compact-operation-failures ()
  "Test failed summary requests preserve context and release command progress."
  (dolist (result
            (list (agent-test-result "empty-summary" nil)
                  (make-condition 'provider-protocol-error
                                  :message "The compaction request failed."
                                  :status 503 :code nil :request-id nil
                                  :response-id nil :response nil)))
    (let ((provider
            (make-instance
             'native-scripted-provider
             :native-items (list (json-object "type" "context_compaction"
                                             "encrypted_content" "uncommitted-checkpoint"))
             :results (list result))))
      (application-operation-tests--call-with-compaction-provider
       provider
       (lambda (application)
         (let ((conversation (application-conversation application)))
           (conversation-append-user-message conversation "Context to preserve.")
           (let* ((records-before (conversation--read-records
                                  (conversation-pathname conversation)))
                  (input-before (json-encode
                                 (coerce (conversation-input-items conversation) 'vector)))
                  (failure
                    (handler-case
                        (progn (application-command application "/compact") nil)
                      (provider-protocol-error (condition)
                        condition))))
             (test-assert failure "compaction failures use the existing typed provider boundary")
             (test-assert
              (and (equal records-before
                          (conversation--read-records (conversation-pathname conversation)))
                   (string= input-before
                            (json-encode (coerce (conversation-input-items conversation) 'vector))))
              "a failed summary does not publish even a successful native checkpoint")
             (test-assert
              (and (not (terminal-ui-compacting-p (application-ui application)))
                   (null (terminal-ui-status (application-ui application))))
              "failed compaction clears its progress indicators")))))))
  nil)
