(in-package #:autolith)

;;;; -- Durable User Operation Context Tests --

(-> user-operation-context-tests--source (list) string)
(defun user-operation-context-tests--source (record)
  "Return the source string from user-operation RECORD."
  (getf (rest record) :source))

(-> user-operation-context-tests--invalid-replay-p
    (configuration string list)
    boolean)
(defun user-operation-context-tests--invalid-replay-p
    (configuration identifier properties)
  "Persist PROPERTIES as one raw user operation and report replay rejection."
  (let ((conversation
          (conversation-create configuration :identifier identifier)))
    (conversation-append-record
     conversation (list* :user-operation properties))
    (handler-case
        (progn
          (conversation-load (conversation-pathname conversation))
          nil)
      (conversation-invariant-error ()
        t))))

(-> test-user-operation-persistence-and-context () null)
(defun test-user-operation-persistence-and-context ()
  "Test bounded durable user-operation replay and request-local projection."
  (let* ((configuration (test-configuration))
         (root (test-configuration-root configuration))
         (conversation
           (conversation-create configuration
                                :identifier "user-operation-context")))
    (unwind-protect
         (progn
           (conversation-append-user-operation
            conversation
            :kind ':lisp
            :source "(setf *example* 1)"
            :status ':ok
            :result "⇒ 1")
           (conversation-append-user-operation
            conversation
            :kind ':command
            :source "/help"
            :status ':ok
            :result "loop action: continue")
           (let* ((records (conversation-user-operation-snapshot conversation))
                  (provider-items-before
                    (copy-tree (conversation-input-items conversation)))
                  (*context-contributors* nil)
                  (*context-resolver*
                    (cl-llm-provider-api:make-context-resolver))
                  (*context-last-deliveries*
                    (make-hash-table :test #'equal))
                  (*context-last-delivery-order* nil))
             (test-assert
              (and (= (length records) 2)
                   (equal (mapcar #'user-operation-context-tests--source records)
                          '("(setf *example* 1)" "/help"))
                   (every (lambda (record)
                            (eq (first record) ':user-operation))
                          records)
                   (null provider-items-before))
              "local operations persist outside ordinary provider input history")
             (setf (char (user-operation-context-tests--source (first records)) 0)
                   #\X)
             (test-assert
              (string= (user-operation-context-tests--source
                        (first
                         (conversation-user-operation-snapshot conversation)))
                       "(setf *example* 1)")
              "user-operation snapshots detach mutable source strings")
             (register-context-contributor "recent-user-operations"
                                           'user-operation-context
                                           :source ':built-in)
             (let* ((delivery
                      (context-resolve-request configuration conversation #()))
                    (contribution
                      (find "recent-user-operations"
                            (context-delivery-contributions delivery)
                            :test #'string=
                            :key #'context-contribution-identifier))
                    (evidence
                      (and contribution
                           (context-contribution-evidence contribution))))
               (test-assert
                (and contribution
                     (eq (context-contribution-class contribution) ':mandatory)
                     (search "outside normal provider conversation history"
                             (context-contribution-instruction contribution))
                     (< (search "/help" evidence)
                        (search "(setf *example* 1)" evidence))
                     (equal provider-items-before
                            (conversation-input-items conversation)))
                "request context exposes newest-first untrusted operation evidence without mutating provider history"))
             (let ((*user-operation-context-evidence-limit* 80))
               (let ((contribution
                       (user-operation-context
                        (make-instance
                         'request-context
                         :configuration configuration
                         :conversation conversation
                         :tool-namespaces #()))))
                 (test-assert
                  (= (length (context-contribution-evidence contribution)) 80)
                  "user-operation evidence obeys its exact character bound")))
             (let ((records
                     (list
                      (list :user-operation
                            :seq 1
                            :time (get-universal-time)
                            :kind ':lisp
                            :source (make-string 4000 :initial-element #\s)
                            :status ':ok
                            :result (make-string 4000 :initial-element #\r)))))
               (let ((*user-operation-context-evidence-limit* 5000))
                 (test-assert
                  (= (length (user-operation-context--evidence records))
                     *context-contribution-evidence-limit*)
                  "operation evidence cannot exceed the context protocol bound"))
               (let ((*user-operation-context-evidence-limit* -1))
                 (test-assert
                  (= (length (user-operation-context--evidence records)) 1900)
                  "an invalid live evidence limit falls back without suppressing context")))
             (test-assert
              (null
               (user-operation-context
                (make-instance
                 'request-context
                 :configuration configuration
                 :conversation conversation
                 :tool-namespaces #()
                 :compaction-p t)))
              "side-channel compaction omits recent user-operation context"))
           (test-assert
            (and (not (conversation-persisted-p conversation))
                 (null (conversation-storage-active-pathname
                        (conversation-pathname conversation))))
            "local operations alone never persist an unstarted conversation")
           (conversation-append-user-message conversation "first message")
           (let* ((*conversation-user-operation-source-limit* 1)
                  (*conversation-user-operation-result-limit* 0)
                  (loaded
                    (conversation-load
                     (conversation-pathname conversation)))
                  (loaded-records
                    (conversation-user-operation-snapshot loaded)))
             (test-assert
              (and (= (length loaded-records) 2)
                   (equal (mapcar #'user-operation-context-tests--source
                                  loaded-records)
                          '("(setf *example* 1)" "/help"))
                   (= (length (conversation-input-items loaded)) 1))
              "the first durable record carries earlier operations into replay"))
           (let ((empty
                   (conversation-create configuration
                                        :identifier "user-operation-empty")))
             (test-assert
              (null
               (user-operation-context
                (make-instance
                 'request-context
                 :configuration configuration
                 :conversation empty
                 :tool-namespaces #())))
              "an empty conversation contributes no user-operation context")))
      (uiop:delete-directory-tree root
                                  :validate t
                                  :if-does-not-exist ':ignore)))
  nil)

(-> test-user-operation-bounds-and-validation () null)
(defun test-user-operation-bounds-and-validation ()
  "Test exact text bounds, recent-window eviction, and strict durable replay."
  (let* ((configuration (test-configuration))
         (root (test-configuration-root configuration)))
    (unwind-protect
         (progn
           (let* ((conversation
                    (conversation-create configuration
                                         :identifier "user-operation-bounds"))
                  (*conversation-user-operation-source-limit* 40)
                  (*conversation-user-operation-result-limit* 30)
                  (record
                    (conversation-append-user-operation
                     conversation
                     :kind ':lisp
                     :source (make-string 100 :initial-element #\s)
                     :status ':ok
                     :result (make-string 100 :initial-element #\r)))
                  (properties (rest record)))
             (test-assert
              (and (= (length (getf properties :source)) 40)
                   (= (length (getf properties :result)) 30)
                   (uiop:string-suffix-p (getf properties :source)
                                         "... [truncated]")
                   (uiop:string-suffix-p (getf properties :result)
                                         "... [truncated]"))
              "durable operation text truncates within exact configured bounds"))
           (let ((conversation
                   (conversation-create configuration
                                        :identifier "user-operation-zero-bound")))
             (let ((*conversation-user-operation-source-limit* 0))
               (test-assert
                (and
                 (handler-case
                     (progn
                       (conversation-append-user-operation
                        conversation
                        :kind ':lisp
                        :source "(values)"
                        :status ':ok
                        :result "")
                       nil)
                   (configuration-error ()
                     t))
                 (= (conversation-next-sequence conversation) 1)
                 (not (conversation-persisted-p conversation)))
                "an invalid source bound fails before any durable append")))
           (let ((conversation
                   (conversation-create configuration
                                        :identifier "user-operation-zero-count")))
             (conversation-append-user-message conversation "start")
             (let ((*conversation-user-operation-count-limit* 0))
               (conversation-append-user-operation
                conversation
                :kind ':command
                :source "/help"
                :status ':ok
                :result "loop action: continue")
               (let ((loaded
                       (conversation-load
                        (conversation-pathname conversation))))
                 (test-assert
                  (and (conversation-persisted-p conversation)
                       (= (conversation-next-sequence conversation) 3)
                       (null (conversation-user-operation-snapshot conversation))
                       (null (conversation-user-operation-snapshot loaded)))
                  "a zero count limit preserves durable records but retains no context"))))
           (let ((conversation
                   (conversation-create configuration
                                        :identifier "user-operation-zero-characters")))
             (conversation-append-user-message conversation "start")
             (let ((*conversation-user-operation-character-limit* 0))
               (conversation-append-user-operation
                conversation
                :kind ':command
                :source "/help"
                :status ':ok
                :result "loop action: continue")
               (let ((loaded
                       (conversation-load
                        (conversation-pathname conversation))))
                 (test-assert
                  (and (conversation-persisted-p conversation)
                       (= (conversation-next-sequence conversation) 3)
                       (null (conversation-user-operation-snapshot conversation))
                       (null (conversation-user-operation-snapshot loaded)))
                  "a zero character limit preserves durable records but retains no context"))))
           (let ((conversation
                   (conversation-create configuration
                                        :identifier "user-operation-retention")))
             (let ((*conversation-user-operation-count-limit* 2)
                   (*conversation-user-operation-character-limit* 1000))
               (dolist (source '("one" "two" "three"))
                 (conversation-append-user-operation
                  conversation
                  :kind ':command
                  :source source
                  :status ':ok
                  :result "x"))
               (test-assert
                (equal (mapcar #'user-operation-context-tests--source
                               (conversation-user-operation-snapshot conversation))
                       '("two" "three"))
                "the recent operation projection evicts oldest records by count"))
              (let ((*conversation-user-operation-count-limit* 16)
                    (*conversation-user-operation-character-limit* 8))
                (conversation-append-user-operation
                 conversation
                 :kind ':command
                 :source "four"
                 :status ':ok
                 :result "1234")
                (let ((at-limit
                        (conversation-user-operation-snapshot conversation)))
                  (conversation-append-user-operation
                   conversation
                   :kind ':command
                   :source "five"
                   :status ':ok
                   :result "12345")
                  (test-assert
                   (and
                    (equal (mapcar #'user-operation-context-tests--source at-limit)
                           '("four"))
                    (null (conversation-user-operation-snapshot conversation)))
                   "the character budget retains exact fits and evicts overweight records"))))
           (let ((conversation
                   (conversation-create configuration
                                        :identifier "user-operation-future-field")))
             (conversation-append-record
              conversation
              (list :user-operation
                    :kind ':lisp
                    :source "(values :future)"
                    :status ':ok
                    :result "⇒ :FUTURE"
                    :future-field "ignored"))
             (let ((loaded
                     (conversation-load
                      (conversation-pathname conversation))))
               (test-assert
                (equal
                 (mapcar #'user-operation-context-tests--source
                         (conversation-user-operation-snapshot loaded))
                 '("(values :future)"))
                "replay tolerates bounded unknown user-operation fields")))
           (test-assert
            (user-operation-context-tests--invalid-replay-p
             configuration
             "user-operation-invalid-kind"
             (list :kind ':tool
                   :source "invalid"
                   :status ':ok
                   :result ""))
            "replay rejects an unsupported durable user-operation kind")
           (test-assert
            (user-operation-context-tests--invalid-replay-p
             configuration
             "user-operation-oversized"
             (list :kind ':lisp
                   :source
                   (make-string (1+ (* 64 1024))
                                :initial-element #\x)
                   :status ':ok
                   :result ""))
            "replay rejects oversized durable user-operation text")
           (let* ((conversation
                    (conversation-create configuration
                                         :identifier "user-operation-circular"))
                  (record
                    (list :user-operation
                          :seq 2
                          :time (get-universal-time)
                          :kind ':lisp
                          :source "(values)"
                          :status ':ok
                          :result ""))
                  (properties (rest record))
                  (tail (last properties)))
             (conversation-append-user-message conversation "start")
             (conversation-append-user-operation
              conversation
              :kind ':lisp
              :source "(values :valid)"
              :status ':ok
              :result "⇒ :VALID")
             (setf (rest tail) properties)
             (unwind-protect
                  (progn
                    (with-open-file
                        (stream (conversation-log-pathname conversation)
                                :direction ':output
                                :if-exists ':append
                                :external-format ':utf-8)
                      (terpri stream)
                      (write record :stream stream :readably t :circle t)
                      (terpri stream))
                    (test-assert
                     (handler-case
                         (progn
                           (conversation-load
                            (conversation-pathname conversation))
                           nil)
                       (conversation-invariant-error ()
                         t))
                     "replay rejects a circular conversation record without hanging"))
               (setf (rest tail) nil)))
           (let ((conversation
                   (conversation-create configuration
                                        :identifier "user-operation-circular-header")))
             (conversation-append-user-message conversation "start")
             (conversation-append-user-operation
              conversation
              :kind ':lisp
              :source "(values :valid)"
              :status ':ok
              :result "⇒ :VALID")
             (let* ((records
                      (conversation--read-records
                       (conversation-pathname conversation)))
                    (header (copy-list (first records)))
                    (durable-record (second records))
                    (properties (rest header))
                    (tail (last properties)))
               (setf (rest tail) properties)
               (unwind-protect
                    (progn
                      (with-open-file
                          (stream (conversation-log-pathname conversation)
                                  :direction ':output
                                  :if-exists ':supersede
                                  :external-format ':utf-8)
                        (write header :stream stream :readably t :circle t)
                        (terpri stream)
                        (write durable-record :stream stream :readably t)
                        (terpri stream))
                      (test-assert
                       (and
                        (null
                         (conversation-peek-header
                          (conversation-pathname conversation)))
                        (handler-case
                            (progn
                              (conversation-load
                               (conversation-pathname conversation))
                              nil)
                          (conversation-invariant-error ()
                            t)))
                       "header peeking and replay reject a circular header without hanging"))
                 (setf (rest tail) nil))))
           (test-assert
            (user-operation-context-tests--invalid-replay-p
             configuration
             "user-operation-missing-status"
             (list :kind ':lisp
                   :source "(values)"
                   :result ""))
            "replay rejects an incomplete durable user-operation property list"))
      (uiop:delete-directory-tree root
                                  :validate t
                                  :if-does-not-exist ':ignore)))
  nil)


;;;; -- Local Operation Capture Tests --

(-> test-user-operation-capture () null)
(defun test-user-operation-capture ()
  "Test local Lisp and interactive commands enter one bounded operation stream."
  (multiple-value-bind (application root)
      (lisp-machine-tests--application)
    (let* ((ui (application-ui application))
           (conversation (application-conversation application))
           (provider-items-before
             (copy-tree (conversation-input-items conversation))))
      (unwind-protect
           (progn
             (terminal-ui-start ui)
             (test-assert
              (eq
               (application-run-lisp-input
                application
                "(progn (format t \"hello~%\") (values 1 2))")
               ':continue)
              "successful local Lisp returns its ordinary loop action")
             (test-assert
              (eq (application-run-lisp-input application "(error \"stop\")")
                  ':aborted)
              "aborted local Lisp retains its debugger outcome")
             (test-call-with-function-replacements
              (list
               (list 'application-lisp-evaluate
                     (lambda (source &key restart-selector application)
                       (declare (ignore source restart-selector application))
                       (application-lisp-evaluation-create
                        :status ':error
                        :output ""
                        :values nil
                        :condition "synthetic failure"
                        :restart-names nil))))
              (lambda ()
                (test-assert
                 (eq
                  (application-run-lisp-input application "(synthetic-error)")
                  ':failed)
                 "unrecoverable local Lisp retains its error outcome")))
             (test-assert
              (eq (application-command application "/help") ':continue)
              "ordinary slash execution records a successful command")
             (let* ((controller (lisp-machine-tests--controller application))
                    (invocation
                      (application-command-invocation-parse "/help"))
                    (command
                      (application-command-invocation-command invocation)))
               (test-call-with-function-replacements
                (list
                 (list
                  'application-input-controller--call-with-responsive-prompt-block
                  (lambda (observed-controller function)
                    (declare (ignore observed-controller))
                    (funcall function))))
                (lambda ()
                  (test-assert
                   (eq
                    (application-input-controller--run-responsive-command
                     controller command invocation)
                    ':continue)
                   "responsive active-turn commands share durable capture")))
               (let ((count-before
                       (length
                        (conversation-user-operation-snapshot conversation))))
                 (test-assert
                  (eq
                   (application-command-execute command application invocation)
                   ':continue)
                  "noninteractive command execution still succeeds")
                 (test-assert
                  (= (length
                      (conversation-user-operation-snapshot conversation))
                     count-before)
                  "noninteractive command calls do not create user operations")))
             (test-assert
              (eq (application-run-lisp-input application "(help)") ':continue)
              "canonical commands execute inside explicit local Lisp")
             (let* ((records
                      (conversation-user-operation-snapshot conversation))
                    (properties (mapcar #'rest records))
                    (success (first properties))
                    (aborted (second properties))
                    (failed (third properties)))
               (test-assert
                (and (= (length records) 6)
                     (equal (mapcar (lambda (record)
                                      (getf record :kind))
                                    properties)
                            '(:lisp :lisp :lisp :command :command :lisp))
                     (equal (mapcar (lambda (record)
                                      (getf record :status))
                                    properties)
                            '(:ok :aborted :error :ok :ok :ok))
                     (equal (mapcar (lambda (record)
                                      (getf record :source))
                                    properties)
                            '("(progn (format t \"hello~%\") (values 1 2))"
                              "(error \"stop\")"
                              "(synthetic-error)"
                              "/help"
                              "/help"
                              "(help)")))
                "local Lisp and slash paths retain exact source, kind, and status once")
               (test-assert
                (and (search "output" (getf success :result))
                     (search "hello" (getf success :result))
                     (search "⇒ 1" (getf success :result))
                     (search "⇒ 2" (getf success :result))
                     (search "aborted: stop" (getf aborted :result)
                             :test #'char-equal)
                     (search "error: synthetic failure" (getf failed :result)
                             :test #'char-equal)
                     (every
                      (lambda (record)
                        (string= (getf record :result)
                                 "loop action: continue"))
                      (subseq properties 3 5)))
                "captured results preserve output, values, conditions, and command actions")
               (test-assert
                (equal provider-items-before
                       (conversation-input-items conversation))
                "captured local operations never become ordinary provider input items")))
        (ignore-errors (terminal-ui-stop ui))
        (ignore-errors
          (tool-registry-close-runtime-state
           (application-tool-registry application)))
        (uiop:delete-directory-tree root
                                    :validate t
                                    :if-does-not-exist ':ignore))))
  nil)

(-> test-user-operation-command-outcomes () null)
(defun test-user-operation-command-outcomes ()
  "Test terminal command abort, expected failure, and quit capture."
  (let ((snapshot (application-command--registry-snapshot)))
    (unwind-protect
         (let ((abort-command
                 (application-command-create
                  :definition-name
                  'user-operation-context-tests--aborting-command
                  :name "/operation-abort"
                  :aliases nil
                  :argument "VALUE"
                  :description "abort for user-operation capture tests"
                  :tip "tests aborted command capture."
                  :busy-behavior ':execute
                  :terminal-behavior ':shared
                   :lambda-list '(value)
                   :callable-p t
                  :handler
                  (lambda (application value)
                    (declare (ignore application value))
                    ':continue)))
               (failure-command
                 (application-command-create
                  :definition-name
                  'user-operation-context-tests--failing-command
                  :name "/operation-failure"
                  :aliases nil
                  :argument nil
                  :description "fail for user-operation capture tests"
                  :tip "tests failed command capture."
                  :busy-behavior ':execute
                  :terminal-behavior ':shared
                   :lambda-list '()
                   :callable-p t
                  :handler
                  (lambda (application)
                    (declare (ignore application))
                    (error 'configuration-error
                           :message "synthetic command failure")))))
           (register-application-command abort-command)
           (register-application-command failure-command)
           (multiple-value-bind (application root)
               (lisp-machine-tests--application)
             (let* ((ui (application-ui application))
                    (controller (lisp-machine-tests--controller application)))
               (unwind-protect
                    (progn
                      (terminal-ui-start ui)
                      (test-assert
                       (eq (application--run-command-input
                            application "/operation-abort")
                           ':aborted)
                       "idle command cancellation returns its debugger outcome")
                      (let* ((abort-invocation
                               (application-command-invocation-parse
                                "/operation-abort"))
                             (abort-command
                               (application-command-invocation-command
                                abort-invocation))
                             (failure-invocation
                               (application-command-invocation-parse
                                "/operation-failure"))
                             (quit-invocation
                               (application-command-invocation-parse "/quit"))
                             (quit-command
                               (application-command-invocation-command
                                quit-invocation)))
                        (test-call-with-function-replacements
                         (list
                          (list
                           'application-input-controller--call-with-responsive-prompt-block
                           (lambda (observed-controller function)
                             (declare (ignore observed-controller))
                             (funcall function))))
                         (lambda ()
                           (test-assert
                            (eq
                             (application-input-controller--run-responsive-command
                              controller abort-command abort-invocation)
                             ':aborted)
                            "responsive command cancellation returns its debugger outcome")
                           (test-assert
                            (eq
                             (application-input-controller--run-responsive-command
                              controller failure-command failure-invocation)
                             ':failed)
                            "responsive expected failures return their command outcome")
                           (test-assert
                            (eq
                             (application-input-controller--run-responsive-command
                              controller quit-command quit-invocation)
                             ':quit)
                            "responsive quit preserves its successful loop action"))))
                      (let* ((records
                               (conversation-user-operation-snapshot
                                (application-conversation application)))
                             (properties (mapcar #'rest records)))
                        (test-assert
                         (and (= (length records) 4)
                              (equal
                               (mapcar (lambda (record)
                                         (getf record :source))
                                       properties)
                               '("/operation-abort" "/operation-abort"
                                 "/operation-failure" "/quit"))
                              (equal
                               (mapcar (lambda (record)
                                         (getf record :status))
                                       properties)
                               '(:aborted :aborted :error :ok)))
                         "terminal command outcomes retain exact source and final status")
                        (test-assert
                         (and
                          (every
                           (lambda (record)
                             (search "invalid number of arguments"
                                     (getf record :result)
                                     :test #'char-equal))
                           (subseq properties 0 2))
                          (string= (getf (third properties) :result)
                                   "error: synthetic command failure")
                          (string= (getf (fourth properties) :result)
                                   "loop action: quit"))
                         "terminal command outcomes retain bounded local diagnostics")))
                 (ignore-errors (terminal-ui-stop ui))
                 (ignore-errors
                   (tool-registry-close-runtime-state
                    (application-tool-registry application)))
                 (uiop:delete-directory-tree
                  root :validate t :if-does-not-exist ':ignore)))))
      (application-command--registry-restore snapshot)))
  nil)

(-> test-user-operation-conversation-switch () null)
(defun test-user-operation-conversation-switch ()
  "Test a successful conversation command records into its resulting target."
  (multiple-value-bind (application root)
      (lisp-machine-tests--application)
    (let ((ui (application-ui application))
          (original-conversation (application-conversation application)))
      (unwind-protect
           (progn
             (terminal-ui-start ui)
             (let ((result nil)
                   (replacement-conversation nil))
               (test-call-with-function-replacements
                (list
                 (list
                  'application-install-conversation
                  (lambda (observed-application conversation)
                    (setf (application-conversation observed-application)
                          conversation)
                    observed-application)))
                (lambda ()
                  (setf result (application-command application "/new")
                        replacement-conversation
                        (application-conversation application))))
               (let ((records
                       (conversation-user-operation-snapshot
                        replacement-conversation)))
                 (test-assert
                  (and (eq result ':continue)
                       (not (eq replacement-conversation
                                original-conversation))
                       (null
                        (conversation-user-operation-snapshot
                         original-conversation))
                       (= (length records) 1)
                       (eq (getf (rest (first records)) :kind) ':command)
                       (eq (getf (rest (first records)) :status) ':ok)
                       (string= (getf (rest (first records)) :source) "/new")
                       (string= (getf (rest (first records)) :result)
                                "loop action: continue"))
                  "conversation-changing commands retain evidence in the new conversation"))))
        (ignore-errors (terminal-ui-stop ui))
        (ignore-errors
          (tool-registry-close-runtime-state
           (application-tool-registry application)))
        (uiop:delete-directory-tree root
                                    :validate t
                                    :if-does-not-exist ':ignore))))
  nil)

(-> test-user-operation-retention-failure () null)
(defun test-user-operation-retention-failure ()
  "Test completed Lisp remains visible when durable operation retention fails."
  (multiple-value-bind (application root)
      (lisp-machine-tests--application)
    (let* ((ui (application-ui application))
           (terminal (terminal-ui-terminal ui))
           (conversation (application-conversation application))
           (propagated-p nil))
      (unwind-protect
           (progn
             (terminal-ui-start ui)
             (setf *lisp-machine-test-value* ':completed
                   *lisp-machine-test-activity* nil)
             (test-call-with-function-replacements
              (list
               (list
                'conversation-append-user-operation
                (lambda (observed-conversation &rest arguments)
                  (declare (ignore arguments))
                  (error 'conversation-invariant-error
                         :message "synthetic user-operation retention failure"
                         :pathname
                         (conversation-pathname observed-conversation)
                         :sequence nil))))
              (lambda ()
                (handler-case
                    (application-run-lisp-input
                     application
                     "(progn (setf *lisp-machine-test-activity* :changed) *lisp-machine-test-value*)")
                  (conversation-invariant-error ()
                    (setf propagated-p t)))))
             (test-assert
              (and propagated-p
                   (eq *lisp-machine-test-activity* ':changed)
                   (search ":COMPLETED"
                           (recording-terminal-output terminal))
                   (null (conversation-user-operation-snapshot conversation)))
              "a retention failure propagates after completed side effects and results are visible"))
        (setf *lisp-machine-test-value* nil
              *lisp-machine-test-activity* nil)
        (ignore-errors (terminal-ui-stop ui))
        (ignore-errors
          (tool-registry-close-runtime-state
           (application-tool-registry application)))
        (uiop:delete-directory-tree root
                                    :validate t
                                    :if-does-not-exist ':ignore))))
  nil)

(-> run-user-operation-context-tests () null)
(defun run-user-operation-context-tests ()
  "Run durable local user-operation projection tests."
  (test-user-operation-persistence-and-context)
  (test-user-operation-bounds-and-validation)
  (test-user-operation-capture)
  (test-user-operation-command-outcomes)
  (test-user-operation-conversation-switch)
  (test-user-operation-retention-failure)
  nil)
