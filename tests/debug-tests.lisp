(in-package #:autolith)

;;;; -- Optional Debug Product Boundary Tests --

(-> debug-tests--adapter () (values string list))
(defun debug-tests--adapter ()
  "Use Daphne's actual subprocess fixture from its installed published system."
  (let* ((root  (asdf:system-source-directory "daphne"))
         (setup (or (uiop:getenv "DAPHNE_TEST_SETUP")
                    (namestring (merge-pathnames ".qlot/setup.lisp" (asdf:system-source-directory "autolith"))))))
    (values (namestring sb-ext:*runtime-pathname*)
            (list "--noinform" "--script" (namestring (merge-pathnames "fixture.lisp" root))
                  setup (namestring (merge-pathnames "daphne.asd" root))))))

(-> debug-tests--fixture (function &key (:authorization (option function)) (:cancel-p (option function))) t)
(defun debug-tests--fixture (function &key authorization cancel-p)
  "Provide an ordinary registry and isolated conversation with actual adapter execution."
  (with-test-configuration (configuration root)
    (let* ((configuration (configuration-copy configuration :working-directory root))
           (registry      (make-instance 'tool-registry))
           (commands      nil)
           (context       (make-instance 'tool-context :configuration configuration :registry registry :worker nil
                                         :conversation (conversation-create configuration :identifier "debug")
                                         :command-authorization-function
                                         (lambda (command directory)
                                           (push command commands)
                                           (if authorization (funcall authorization command directory) ':full-access)))))
      (multiple-value-bind (program arguments) (debug-tests--adapter)
        (debug-register-tools registry :program program :arguments arguments :cancel-p cancel-p))
      (unwind-protect
           (funcall function context (lambda () commands))
        (tool-registry-close-runtime-state registry)))))

(-> debug-tests--invoke (tool-context string hash-table) tool-result)
(defun debug-tests--invoke (context operation arguments)
  "Invoke through normal tool lookup, input validation and error projection."
  (tool-registry-execute-call (tool-context-registry context)
                              (json-object "namespace" "debug" "name" operation "arguments" (json-encode arguments)) context))

(-> debug-tests--call (tool-context string hash-table) t)
(defun debug-tests--call (context operation arguments)
  "Decode a successful ordinary tool result."
  (let ((result (debug-tests--invoke context operation arguments)))
    (test-assert (tool-result-success-p result) (format nil "debug.~A succeeds: ~A" operation (tool-result-content result)))
    (json-decode (tool-result-content result))))

(-> debug-tests--session (tool-context &key (:mode string)) string)
(defun debug-tests--session (context &key (mode "launch"))
  "Start a real fixture adapter using the public semantic surface."
  (json-get (debug-tests--call context mode (json-object "configuration" "{}")) "session"))

(-> debug-tests--entry (tool-context string) debug-session-entry)
(defun debug-tests--entry (context identifier)
  "Retain transport solely for product lifecycle assertions."
  (debug--find (debug-tool-manager (tool-registry-find (tool-context-registry context) "debug" "status")) context identifier))

(-> debug-tests--reaped (debug-session-entry) null)
(defun debug-tests--reaped (entry)
  "Verify actual adapter death and wait completion after product cleanup."
  (let ((process (daphne:adapter-process (debug-session-entry-transport entry))))
    (test-assert (not (sb-ext:process-alive-p process)) "owned adapter is no longer alive")
    (test-assert (member (sb-ext:process-status process) '(:exited :signaled)) "owned adapter has a terminal reaped status")))

(-> test-debug-semantic-lifecycle () null)
(defun test-debug-semantic-lifecycle ()
  "Use launch/attach, source authority and every semantic operation through ordinary invocation."
  (debug-tests--fixture
   (lambda (context commands)
     (let* ((identifier (debug-tests--session context))
            (entry      (debug-tests--entry context identifier))
            (base       (json-object "session" identifier)))
       (test-assert (json-get (debug-tests--call context "status" (json-object)) "configured") "optional adapter configured")
       (test-assert (plusp (length (tool-registry-provider-schemas (tool-context-registry context)))) "optional provider schemas available")
       (test-assert (find-if (lambda (command) (uiop:string-prefix-p "debug.launch " command)) (funcall commands)) "launch independently authorized")
       (test-assert (find-if (lambda (command) (uiop:string-prefix-p "debug.adapter " command)) (funcall commands)) "adapter executable and arguments authorized")
       (debug-tests--call context "events" (json-object "session" identifier "wait-for" "stopped" "count" 0))
       (let ((path (merge-pathnames "source.lisp" (config :working-directory (tool-context-configuration context)))))
         (with-open-file (stream path :direction ':output :if-does-not-exist ':create) (write-line "(print 42)" stream))
         (test-assert (json-get (debug-tests--call context "breakpoints" (json-object "session" identifier "path" (namestring path) "lines" #(3))) "breakpoints") "semantic source breakpoints"))
       (test-assert (= 1 (json-get (aref (json-get (debug-tests--call context "threads" base) "threads") 0) "id")) "thread identity")
       (test-assert (= 2 (json-get (aref (json-get (debug-tests--call context "stack" (json-object "session" identifier "thread" 1 "count" 2)) "stackFrames") 0) "id")) "frame identity")
       (test-assert (= 4 (json-get (aref (json-get (debug-tests--call context "scopes" (json-object "session" identifier "frame" 2)) "scopes") 0) "variablesReference")) "scope identity")
       (test-assert (equal "42" (json-get (aref (json-get (debug-tests--call context "variables" (json-object "session" identifier "reference" 4 "count" 2)) "variables") 0) "value")) "variable page")
       (test-assert (equal "λ:42" (json-get (debug-tests--call context "evaluate" (json-object "session" identifier "expression" "answer" "frame" 2)) "result")) "explicit expression result")
       (test-assert (find-if (lambda (command) (and (uiop:string-prefix-p "debug.evaluate " command) (search "answer" command))) (funcall commands)) "exact expression independently approved")
       (dolist (kind '("in" "over" "out"))
         (debug-tests--call context "step" (json-object "session" identifier "thread" 1 "kind" kind)))
       (debug-tests--call context "continue" (json-object "session" identifier "thread" 1))
       (debug-tests--call context "pause" (json-object "session" identifier "thread" 1))
       (let ((page (debug-tests--call context "events" (json-object "session" identifier "count" 1))))
         (test-assert (= 1 (length (json-get page "events"))) "finite event page")
         (test-assert (plusp (json-get page "bufferedRemaining")) "unpresented real events retained")
         (debug-tests--call context "events" (json-object "session" identifier "wait-for" "stopped"))
         (test-assert (vectorp (json-get (debug-tests--call context "events" base) "events")) "remaining event drain"))
       (let ((terminate (symbol-function 'daphne:session-terminate))
             (forwarded nil))
         (test-call-with-function-replacements
          (list (list 'daphne:session-terminate
                      (lambda (session &rest options)
                        (push (getf options :terminate-debuggee) forwarded)
                        (apply terminate session options))))
          (lambda ()
            (test-assert (eq ':invalid-arguments
                             (tool-result-error-code
                              (debug-tests--invoke context "terminate" (json-object "session" identifier "terminate-debuggee" "false"))))
                         "malformed termination flag rejected before disconnect")
            (debug-tests--call context "terminate" base)
            (debug-tests--reaped entry)
            (test-assert (eq ':unknown-session (tool-result-error-code (debug-tests--invoke context "threads" base))) "terminated session handle consumed")
            (let* ((started        (debug-tests--call context "attach"
                                                     (json-object "configuration" "{}" "breakpoint-path" "source.lisp"
                                                                  "breakpoint-lines" #(3))))
                   (attached       (json-get started "session"))
                   (attached-entry (debug-tests--entry context attached)))
              (test-assert (json-get (aref (json-get (json-get started "breakpoints") "breakpoints") 0) "verified")
                           "initial breakpoints verified before configuration completion")
              (test-assert (find-if (lambda (command) (uiop:string-prefix-p "debug.attach " command)) (funcall commands)) "attach separately approved")
              (debug-tests--call context "terminate" (json-object "session" attached "terminate-debuggee" (json-false)))
              (debug-tests--reaped attached-entry))
            (let* ((null-session (debug-tests--session context))
                   (null-entry   (debug-tests--entry context null-session)))
              (debug-tests--call context "terminate" (json-object "session" null-session "terminate-debuggee" nil))
              (debug-tests--reaped null-entry))))
         (test-assert (equal '(t nil nil) (nreverse forwarded)) "absent, false and null flags reach disconnect as true, false and false")))))
  nil)

(-> test-debug-authority-and-scoping () null)
(defun test-debug-authority-and-scoping ()
  "Deny adapter/attach/expression authority, enforce session ownership and fail unavailable configuration."
  (debug-tests--fixture
   (lambda (context commands)
     (declare (ignore commands))
     (test-assert (eq ':authorization (tool-result-error-code (debug-tests--invoke context "attach" (json-object "configuration" "{\"processId\":12}")))) "attach denied despite configured adapter")
     (let* ((identifier    (debug-tests--session context))
            (entry         (debug-tests--entry context identifier))
            (configuration (tool-context-configuration context))
            (foreign       (make-instance 'tool-context :configuration configuration :registry (tool-context-registry context) :worker nil
                                                        :conversation (conversation-create configuration :identifier "other"))))
       (dolist (operation '("threads" "cancel" "terminate"))
         (test-assert (eq ':unknown-session (tool-result-error-code (debug-tests--invoke foreign operation (json-object "session" identifier)))) "cross-conversation session inaccessible"))
       (test-assert (eq ':authorization (tool-result-error-code (debug-tests--invoke context "evaluate" (json-object "session" identifier "expression" "mutate()")))) "evaluation denied independently")
       (debug-tests--call context "threads" (json-object "session" identifier))
       (debug-tests--call context "terminate" (json-object "session" identifier))
       (debug-tests--reaped entry))
     (debug-register-tools (tool-context-registry context) :program nil)
     (test-assert (eq ':unavailable (tool-result-error-code (debug-tests--invoke context "launch" (json-object "configuration" "{}")))) "optional adapter unavailable is typed")
     (test-assert (eq ':invalid-arguments (tool-result-error-code (debug-tests--invoke context "launch" (json-object "configuration" "[]")))) "non-object adapter configuration rejected"))
   :authorization (lambda (command directory)
                    (declare (ignore directory))
                    (if (or (uiop:string-prefix-p "debug.attach " command)
                            (uiop:string-prefix-p "debug.evaluate " command)) ':deny ':full-access)))
  (debug-tests--fixture
   (lambda (context commands)
     (declare (ignore commands))
     (test-assert (not (tool-result-success-p (debug-tests--invoke context "launch" (json-object "configuration" "{}")))) "sandbox-only adapter authority rejected")
     (test-assert (zerop (hash-table-count (debug-session-manager-entries
                                            (debug-tool-manager (tool-registry-find (tool-context-registry context) "debug" "status"))))) "denial admits no adapter"))
   :authorization (lambda (command directory) (declare (ignore command directory)) ':sandboxed))
  nil)

(-> test-debug-cancellation-and-timeout () null)
(defun test-debug-cancellation-and-timeout ()
  "Cancel a real blocked ordinary invocation and verify deadline/supervisor adapter reaping."
  (debug-tests--fixture
   (lambda (context commands)
     (declare (ignore commands))
     (let* ((identifier (debug-tests--session context))
            (entry      (debug-tests--entry context identifier))
            (result     nil)
            (thread     (sb-thread:make-thread
                         (lambda () (setf result (debug-tests--invoke context "events"
                                                                      (json-object "session" identifier "wait-for" "never" "timeout" 30)))))))
       (unwind-protect
            (progn
              (loop repeat 100 until (debug-session-entry-busy-p entry) do (sleep 0.01))
              (test-assert (debug-session-entry-busy-p entry) "blocking call holds session reservation")
              (test-assert (eq ':session-busy (tool-result-error-code (debug-tests--invoke context "threads" (json-object "session" identifier)))) "competing operation fails without closing active call")
              (debug-tests--call context "cancel" (json-object "session" identifier))
              (sb-thread:join-thread thread :timeout 5)
              (test-assert (eq ':cancelled (tool-result-error-code result)) "cancel interrupts blocked request")
              (debug-tests--reaped entry))
         (when (sb-thread:thread-alive-p thread)
           (tool-runtime-close (tool-registry-find (tool-context-registry context) "debug" "status")))
         (sb-thread:join-thread thread :timeout 5)))
     (let* ((identifier (debug-tests--session context))
            (entry      (debug-tests--entry context identifier)))
       (test-assert (eq ':timeout (tool-result-error-code (debug-tests--invoke context "events" (json-object "session" identifier "wait-for" "never" "timeout" 1)))) "deadline projects typed timeout")
       (debug-tests--reaped entry))))
  (let ((cancelled nil))
    (debug-tests--fixture
     (lambda (context commands)
       (declare (ignore commands))
       (let* ((identifier (debug-tests--session context))
              (entry      (debug-tests--entry context identifier)))
         (setf cancelled t)
         (test-assert (eq ':cancelled (tool-result-error-code (debug-tests--invoke context "threads" (json-object "session" identifier)))) "supervisor predicate cancels actual adapter request")
         (debug-tests--reaped entry)))
     :cancel-p (lambda () cancelled)))
  (let ((cancelled nil))
    (debug-tests--fixture
     (lambda (context commands)
       (declare (ignore commands))
       (let* ((identifier (debug-tests--session context))
              (entry      (debug-tests--entry context identifier)))
         (setf (debug-session-entry-events entry)
               (list (json-object "event" "stopped" "body" (json-object))))
         (setf cancelled t)
         (test-assert (eq ':cancelled
                          (tool-result-error-code
                           (debug-tests--invoke context "events" (json-object "session" identifier "wait-for" "stopped"))))
                      "cached events observe supervisor cancellation")
         (debug-tests--reaped entry)))
     :cancel-p (lambda () cancelled)))
  (let ((starts 0)
        (start  (symbol-function 'daphne:start-adapter)))
    (test-call-with-function-replacements
     (list (list 'daphne:start-adapter
                 (lambda (&rest arguments)
                   (incf starts)
                   (apply start arguments))))
     (lambda ()
       (debug-tests--fixture
        (lambda (context commands)
          (declare (ignore commands))
          (test-assert (eq ':cancelled
                           (tool-result-error-code
                            (debug-tests--invoke context "launch" (json-object "configuration" "{}"))))
                       "already-cancelled supervisor rejects startup")
          (test-assert (zerop starts) "cancelled startup performs no adapter process effect"))
        :cancel-p (constantly t)))))
  nil)

(-> test-debug-runtime-and-bounds () null)
(defun test-debug-runtime-and-bounds ()
  "Enforce admission/input/result bounds and close actual adapters through registry lifecycle."
  (debug-tests--fixture
   (lambda (context commands)
     (let ((before (length (funcall commands))))
       (test-assert (eq ':invalid-arguments
                        (tool-result-error-code (debug-tests--invoke context "launch" (json-object "configuration" "{}" "timeout" 0))))
                    "startup contracts reject before adapter authorization")
       (test-assert (= before (length (funcall commands))) "invalid startup performs no authorization effect"))
     (let* ((*debug-conversation-session-limit* 1)
            (registry                           (tool-context-registry context))
            (identifier                         (debug-tests--session context))
            (entry                              (debug-tests--entry context identifier)))
       (test-assert (eq ':session-limit (tool-result-error-code (debug-tests--invoke context "launch" (json-object "configuration" "{}")))) "capacity rejects without eviction")
       (let ((before (length (funcall commands))))
         (test-assert (eq ':invalid-arguments (tool-result-error-code (debug-tests--invoke context "variables" (json-object "session" identifier "reference" 4 "count" 101)))) "semantic page contract checked")
         (test-assert (eq ':invalid-arguments (tool-result-error-code (debug-tests--invoke context "breakpoints" (json-object "session" identifier "path" "source.lisp" "lines" #(0))))) "breakpoint lines validated")
         (test-assert (= before (length (funcall commands))) "invalid fields precede path authorization")
         (test-assert (eq entry (debug-tests--entry context identifier)) "invalid fields do not reserve or remove the live session"))
       (dolist (wait-for (list nil (json-false)))
         (test-assert (vectorp (json-get (debug-tests--call context "events" (json-object "session" identifier "wait-for" wait-for)) "events"))
                      "false and null wait-for select queued pages"))
       (let ((stack     (symbol-function 'daphne:session-stack-trace))
             (variables (symbol-function 'daphne:session-variables))
             (evaluate  (symbol-function 'daphne:session-evaluate))
             (seen      nil))
         (test-call-with-function-replacements
          (list (list 'daphne:session-step
                      (lambda (&rest arguments)
                        (declare (ignore arguments))
                        (push (cons "step" nil) seen)))
                (list 'daphne:session-stack-trace
                      (lambda (session thread &rest options)
                        (push (cons "stack" options) seen)
                        (apply stack session thread options)))
                (list 'daphne:session-variables
                      (lambda (session reference &rest options)
                        (push (cons "variables" options) seen)
                        (apply variables session reference options)))
                (list 'daphne:session-evaluate
                      (lambda (session expression &rest options)
                        (push (cons "evaluate" options) seen)
                        (apply evaluate session expression options))))
          (lambda ()
            (test-assert (eq ':invalid-arguments
                             (tool-result-error-code (debug-tests--invoke context "step" (json-object "session" identifier "thread" 1 "kind" "IN"))))
                         "case-sensitive step enum rejects uppercase input")
            (test-assert (not (assoc "step" seen :test #'equal)) "invalid enum sends no DAP request")
            (debug-tests--call context "stack" (json-object "session" identifier "thread" 1))
            (debug-tests--call context "variables" (json-object "session" identifier "reference" 4))
            (debug-tests--call context "evaluate" (json-object "session" identifier "expression" "answer"))
            (dolist (frame (list nil (json-false)))
              (debug-tests--call context "evaluate" (json-object "session" identifier "expression" "answer" "frame" frame)))))
         (let ((stack     (rest (assoc "stack" seen :test #'equal)))
               (variables (rest (assoc "variables" seen :test #'equal)))
               (evaluate  (rest (assoc "evaluate" seen :test #'equal))))
           (test-assert (and (= 0 (getf stack :start-frame)) (= 20 (getf stack :levels))
                             (= 10 (getf stack :timeout))) "stack schema defaults reach the actual adapter call")
           (test-assert (and (= 0 (getf variables :start)) (= 50 (getf variables :count))
                             (= 10 (getf variables :timeout))) "variables schema defaults reach the actual adapter call")
           (test-assert (null (getf evaluate :frame-id)) "optional evaluate frame is not defaulted to zero")))
       (let ((*debug-result-byte-limit* 8))
         (test-assert (eq ':result-limit (tool-result-error-code (debug-tests--invoke context "threads" (json-object "session" identifier)))) "oversize response rejected as bounded typed result"))
       (tool-registry-close-runtime-state registry)
       (debug-tests--reaped entry)
       (test-assert (eq ':closed (tool-result-error-code (debug-tests--invoke context "launch" (json-object "configuration" "{}")))) "closed runtime rejects startup")
       (tool-registry-resume-runtime-state registry)
       (let ((new (debug-tests--session context)))
         (test-assert (not (equal identifier new)) "resumption creates a fresh adapter, never resurrects old session")
         (debug-tests--call context "terminate" (json-object "session" new))))))
  nil)

(-> test-debug-adapter-failure () null)
(defun test-debug-adapter-failure ()
  "Project malformed sessions and actual adapter launch/EOF failures without retaining admission."
  (debug-tests--fixture
   (lambda (context commands)
     (declare (ignore commands))
     (let ((registry (tool-context-registry context)))
       (test-assert (eq ':unknown-session (tool-result-error-code (debug-tests--invoke context "threads" (json-object "session" "absent")))) "unknown session handle is typed")
       (test-assert (eq ':invalid-arguments (tool-result-error-code (debug-tests--invoke context "threads" (json-object "session" 42)))) "non-string session handle is typed")
       (debug-register-tools registry :program "missing-adapter")
       (test-assert (eq ':unavailable (tool-result-error-code (debug-tests--invoke context "launch" (json-object "configuration" "{}")))) "missing executable is typed")
       (multiple-value-bind (program arguments) (debug-tests--adapter)
         (declare (ignore arguments))
         (debug-register-tools registry :program program :arguments '("--noinform" "--non-interactive" "--eval" "(quit)")))
       (test-assert (eq ':adapter-died (tool-result-error-code (debug-tests--invoke context "launch" (json-object "configuration" "{}")))) "actual adapter EOF is typed")
       (test-assert (zerop (hash-table-count (debug-session-manager-entries (debug-tool-manager (tool-registry-find registry "debug" "status"))))) "failed startup releases all admission"))))
  nil)

(-> test-debug-shutdown-admission () null)
(defun test-debug-shutdown-admission ()
  "Shutdown between capacity reservation and execution forbids a late adapter spawn."
  (debug-tests--fixture
   (lambda (context commands)
     (declare (ignore commands))
     (let ((reserved (sb-thread:make-semaphore))
           (release  (sb-thread:make-semaphore))
           (reserve  (symbol-function 'debug--reserve))
           (start    (symbol-function 'daphne:start-adapter))
           (starts   0)
           (thread   nil)
           (result   nil))
       (test-call-with-function-replacements
        (list (list 'debug--reserve
                    (lambda (manager context)
                      (let ((entry (funcall reserve manager context)))
                        (sb-thread:signal-semaphore reserved)
                        (sb-thread:wait-on-semaphore release)
                        entry)))
              (list 'daphne:start-adapter
                    (lambda (&rest arguments)
                      (incf starts)
                      (apply start arguments))))
        (lambda ()
          (unwind-protect
               (progn
                 (setf thread (sb-thread:make-thread
                               (lambda ()
                                 (setf result (debug-tests--invoke context "launch" (json-object "configuration" "{}"))))))
                 (test-assert (sb-thread:wait-on-semaphore reserved :timeout 5) "startup reserved capacity")
                 (tool-registry-close-runtime-state (tool-context-registry context))
                 (sb-thread:signal-semaphore release)
                 (sb-thread:join-thread thread :timeout 5)
                 (test-assert (eq ':cancelled (tool-result-error-code result)) "shutdown cancels pending startup")
                 (test-assert (zerop starts) "closed runtime cannot execute a late adapter"))
            (sb-thread:signal-semaphore release)
            (when thread (sb-thread:join-thread thread :timeout 5))))))))
  nil)

(eval-when (:load-toplevel :execute)
  (define-test-suite debug
    test-debug-semantic-lifecycle
    test-debug-authority-and-scoping
    test-debug-cancellation-and-timeout
    test-debug-runtime-and-bounds
    test-debug-adapter-failure
    test-debug-shutdown-admission))
