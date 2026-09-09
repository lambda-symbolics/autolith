(in-package #:autolith)

;;;; -- Test Runner Behavior --

(-> test-runner-selection () null)
(defun test-runner-selection ()
  "Test exact suite/case selection, deduplication and invalid selectors."
  (let ((*test-suites* '(("first" case-one case-two)
                        ("second" case-three))))
    (test-assert (equal (tests-select) '(case-one case-two case-three))
                 "no selectors selects every case once")
    (test-assert
     (equal (tests-select :suites '("FIRST") :tests '("CASE-TWO" "case-three"))
            '(case-one case-two case-three))
     "suite and case selection form a stable case-insensitive union")
    (test-assert (equal (tests-select :tests '(case-three)) '(case-three))
                 "single case selection does not execute its suite peers")
    (dolist (selectors '((:suites ("missing")) (:tests ("missing"))))
      (test-assert
       (handler-case
           (progn (apply #'tests-select selectors) nil)
         (test-selection-error () t))
       "unknown selectors fail instead of producing a successful empty run")))
  nil)

(-> test-runner-failure-reporting () null)
(defun test-runner-failure-reporting ()
  "Test that assertions and errors are reported while later cases still run."
  (let ((*test-suites* '(("runner-probe" runner-probe-failure
                         runner-probe-error runner-probe-success)))
        (*tests-running-p* t))
    (unwind-protect
         (progn
           (fiveam:test runner-probe-failure
             (test-assert nil "intentional failed assertion"))
           (fiveam:test runner-probe-error
             (error "intentional case error"))
           (fiveam:test runner-probe-success
             (test-assert t "later case executed"))
           (let* ((result (tests-run-cases (tests-select)
                                          :stream (make-broadcast-stream)))
                  (failures (getf result ':failures)))
             (test-assert (= 3 (getf result ':cases))
                          "every case executes despite prior failures")
             (test-assert (= 2 (length failures))
                          "assertion and unexpected error are failed cases")
             (test-assert
              (and (search "intentional failed assertion" (getf (first failures) ':detail))
                   (search "intentional case error" (getf (second failures) ':detail)))
              "failure details retain assertion and condition evidence")
             (test-assert (= 3 (length (getf result ':timings)))
                          "timings include successful and failed cases")
             (test-assert (not (tests-report result (make-broadcast-stream)))
                          "failed cases produce a false report status")))
      (dolist (name '(runner-probe-failure runner-probe-error runner-probe-success))
        (fiveam:rem-test name))))
  nil)

(-> test-runner-temporary-cleanup () null)
(defun test-runner-temporary-cleanup ()
  "Test cleanup around successful, failed and nonlocally exiting FiveAM cases."
  (let* ((*test-suites* '(("cleanup-probe" runner-cleanup-probe)))
         (fixture-root nil)
         (run-root nil)
         (exit nil)
         (previous (sb-ext:symbol-global-value '*test-temporary-root*))
         (*cleanup-probe*
           (lambda ()
             (setf run-root (sb-ext:symbol-global-value '*test-temporary-root*)
                   fixture-root (test-configuration-root (test-configuration)))
             (case exit
               (:throw
                (throw 'runner-exit :escaped))
               (:error
                (error 'test-fixture-error)))
             (test-assert (not (eq exit :failure)) "cleanup probe assertion"))))
    (declare (special *cleanup-probe*))
    (unwind-protect
         (progn
           (fiveam:test (runner-cleanup-probe :compile-at :definition-time)
             (declare (special *cleanup-probe*))
             (funcall *cleanup-probe*))
           (dolist (mode '(:normal :failure :error :throw))
             (setf exit mode)
             (let ((result (catch 'runner-exit
                             (tests-run-cases '(runner-cleanup-probe)
                                              :stream (make-broadcast-stream)))))
               (test-assert
                (if (eq mode :throw)
                    (eq result :escaped)
                    (= (length (getf result ':failures))
                       (if (eq mode :normal) 0 1)))
                "run cleanup preserves successful results, failures and nonlocal exits")
               (test-assert (and fixture-root (not (probe-file fixture-root))
                                 run-root (not (probe-file run-root)))
                            "the runner deletes all unclaimed configuration fixtures")
               (test-assert
                (equal previous (sb-ext:symbol-global-value '*test-temporary-root*))
                "the runner restores an enclosing fixture owner"))))
      (fiveam:rem-test 'runner-cleanup-probe)))
  nil)

(-> test-runner-catalog () null)
(defun test-runner-catalog ()
  "Test that the complete catalog has unique callable FiveAM cases."
  (let ((cases (tests-select)))
    (test-assert (= (length cases) (length (remove-duplicates cases)))
                 "every case belongs to exactly one catalog suite")
    (test-assert (every (lambda (name)
                          (and (fboundp name) (fiveam:get-test name)))
                        cases)
                 "catalog entries resolve to functions and FiveAM tests"))
  nil)

;;;; -- Command-Line Worker Boundary --

(-> test-check--call (string &rest t) t)
(defun test-check--call (name &rest arguments)
  "Call a check-script helper, loading its library surface if necessary."
  (unless (fboundp (find-symbol "CHECK-MAIN" '#:cl-user))
    (progv (list (intern "*CHECK-SCRIPT-LIBRARY-MODE*" '#:cl-user)) '(t)
      (load (merge-pathnames "script/check.lisp"
                            (asdf:system-source-directory :autolith)))))
  (apply #'uiop:symbol-call '#:cl-user name arguments))

(-> test-check-command-selection () null)
(defun test-check-command-selection ()
  "Test CLI parsing and deterministic nonoverlapping worker assignments."
  (let ((options (test-check--call
                  "CHECK--PARSE-ARGUMENTS"
                  '("--suite" "fixtures" "--test" "test-runner-catalog"
                    "--suite" "core" "--jobs" "2" "--timeout" "15"))))
    (test-assert (equal (getf options ':suites) '("fixtures" "core"))
                 "repeated suite options preserve their selections")
    (test-assert (and (= 2 (getf options ':jobs))
                      (= 15 (getf options ':timeout)))
                 "worker limits parse as positive integers"))
  (test-call-with-function-replacements
   (list (list (find-symbol "CHECK--PROCESSOR-COUNT" '#:cl-user)
               (lambda () 7)))
   (lambda ()
     (test-assert (= 7 (getf (test-check--call "CHECK--PARSE-ARGUMENTS" nil) ':jobs))
                  "worker concurrency defaults to the detected CPU count")
     (test-assert
      (= 3 (getf (test-check--call "CHECK--PARSE-ARGUMENTS" '("--jobs" "3")) ':jobs))
      "explicit concurrency overrides CPU detection")))
  ;; Windows publishes the count in the environment; the utilities are the
  ;; path every host takes without it. Windows 11 answers the variable from
  ;; the processor count itself whatever the environment block holds, so the
  ;; utility path is only reachable where removing it actually removes it.
  (with-test-environment (("NUMBER_OF_PROCESSORS" nil))
    (if (uiop:getenv "NUMBER_OF_PROCESSORS")
        (test-withheld ':removable-processor-count
                       "CPU detection through host utilities")
        (progn
          (dolist (case '(("8" 0 8) (" 12 " 0 12) ("0" 0 1) ("-2" 0 1)
                         ("" 0 1) ("unknown" 0 1) ("8 junk" 0 1) ("8" 1 1)))
            (destructuring-bind (output status expected) case
              (test-call-with-function-replacements
               (list (list 'uiop:run-program
                           (lambda (&rest arguments)
                             (declare (ignore arguments))
                             (values output nil status))))
               (lambda ()
                 (test-assert (= expected (test-check--call "CHECK--PROCESSOR-COUNT"))
                              "CPU detection requires successful positive integer output")))))
          (let ((attempts 0))
            (test-call-with-function-replacements
             (list (list 'uiop:run-program
                         (lambda (&rest arguments)
                           (declare (ignore arguments))
                           (if (= (incf attempts) 1)
                               (error 'file-error :pathname #P"missing-cpu-probe")
                               (values "6" nil 0)))))
             (lambda ()
               (test-assert (= 6 (test-check--call "CHECK--PROCESSOR-COUNT"))
                            "CPU detection tries another utility when the first is absent"))))
          (test-call-with-function-replacements
           (list (list 'uiop:run-program
                       (lambda (&rest arguments)
                         (declare (ignore arguments))
                         (error 'file-error :pathname #P"missing-cpu-probe"))))
           (lambda ()
             (test-assert (= 1 (test-check--call "CHECK--PROCESSOR-COUNT"))
                          "unavailable CPU detection falls back to one worker"))))))
  (dolist (arguments '(("--jobs" "0") ("--timeout" "-1") ("--jobs" "2x")
                       ("--suite") ("--unknown")))
    (test-assert
     (handler-case
         (progn (test-check--call "CHECK--PARSE-ARGUMENTS" arguments) nil)
       (error () t))
     "invalid command options fail closed"))
  (dolist (jobs '(1 2 10))
    (let* ((cases '(a b c d e))
           (shards (test-check--call "CHECK--PARTITION-CASES" cases jobs))
           (assigned (apply #'append shards)))
      (test-assert
       (and (= (length shards) (min jobs (length cases)))
            (every #'consp shards)
            (= (length assigned) (length cases))
            (null (set-exclusive-or assigned cases)))
       "each selected case is assigned once within the concurrency limit")))
  nil)

(-> test-check-result-validation () null)
(defun test-check-result-validation ()
  "Test missing, malformed, duplicated and incomplete worker results fail."
  (let ((valid '(:version 1 :cases 1 :checks 2 :failures nil
                :timings (("case-one" 0.1d0)))))
    (test-assert (equal valid (test-check--call "CHECK--VALIDATE-RESULT"
                                               valid '("case-one")))
                 "complete matching worker results are accepted")
    (dolist (invalid
             (list nil
                   (append valid '(:extra t))
                   '(:version 1 :cases 1 :checks 2 :checks 3 :timings nil)
                   '(:version 1 :cases 1 :checks -1 :failures nil
                     :timings (("case-one" 0.1d0)))
                   '(:version 1 :cases 1 :checks 2 :failures nil :timings nil)
                   '(:version 1 :cases 1 :checks 2 :failures nil
                     :timings (("other" 0.1d0)))
                   '(:version 1 :cases 1 :checks 2
                     :failures ((:test "other" :detail "wrong case"))
                     :timings (("case-one" 0.1d0)))))
      (test-assert
       (handler-case
           (progn (test-check--call "CHECK--VALIDATE-RESULT" invalid '("case-one")) nil)
         (error () t))
       "invalid worker results never count as successful coverage")))
  (with-test-configuration (configuration root)
    (declare (ignore configuration))
    (let ((path (merge-pathnames "result.sexp" root)))
      (dolist (content '("(:version 1" "nil nil" "#.(error \"reader evaluation\")"))
        (workspace-resource-tests--write-text path content)
        (test-assert
         (handler-case
             (progn (test-check--call "CHECK--READ-SINGLE-FORM" path) nil)
           (error () t))
         "truncated, trailing, and reader-evaluated worker data are rejected"))))
  nil)

(-> test-check-process-lifecycle () null)
(defun test-check-process-lifecycle ()
  "Test concurrent process admission, exit failures and descendant cleanup."
  (with-test-fixture (':posix-shell "check processes driven by shell commands")
    (test-check--process-lifecycle))
  nil)

(-> test-check--process-lifecycle () null)
(defun test-check--process-lifecycle ()
  "Drive CHECK--RUN-PROCESSES with shell commands and verify each outcome."
  (with-test-configuration (configuration root)
    (declare (ignore configuration))
    (let* ((marker-a (merge-pathnames "a" root))
           (marker-b (merge-pathnames "b" root))
           (late-marker (merge-pathnames "leaked" root))
           (entries
             (loop for own in (list marker-a marker-b)
                   for peer in (list marker-b marker-a)
                   for index from 1
                   collect
                   (test-check--call
                    "MAKE-CHECK-PROCESS"
                    :label "concurrent barrier"
                    :command (list "sh" "-c"
                                   (format nil "touch ~A; while [ ! -e ~A ]; do sleep 0.02; done"
                                           (uiop:escape-shell-token (namestring own))
                                           (uiop:escape-shell-token (namestring peer))))
                    :output (merge-pathnames (format nil "barrier-~D.log" index) root)))))
      (test-check--call "CHECK--RUN-PROCESSES" entries :jobs 2 :timeout 5)
      (test-assert
       (every (lambda (entry)
                (and (eql 0 (test-check--call "CHECK-PROCESS-STATUS" entry))
                     (null (test-check--call "CHECK-PROCESS-PROBLEM" entry))))
              entries)
       "workers reach a mutual barrier concurrently")
      (let ((failure (test-check--call "MAKE-CHECK-PROCESS" :label "failed command"
                                      :command '("sh" "-c" "exit 7")
                                      :output (merge-pathnames "failure.log" root)))
            (timeout (test-check--call
                      "MAKE-CHECK-PROCESS" :label "deadline"
                      :command (list "sh" "-c"
                                     (format nil "(sleep 2; touch ~A) & wait"
                                             (uiop:escape-shell-token
                                              (namestring late-marker))))
                      :output (merge-pathnames "timeout.log" root))))
        (test-check--call "CHECK--RUN-PROCESSES" (list failure timeout) :jobs 2 :timeout 1)
        (test-assert (eql 7 (test-check--call "CHECK-PROCESS-STATUS" failure))
                     "nonzero command exits are retained")
        (test-assert (test-check--call "CHECK-PROCESS-PROBLEM" timeout)
                     "a deadline is reported as a worker failure")
        (test-assert
         (not (uiop:process-alive-p (test-check--call "CHECK-PROCESS-PROCESS" timeout)))
         "the timed-out worker leader is reaped")
        (sleep 1.2)
        (test-assert (not (probe-file late-marker))
                     "timeout terminates descendants before they can write"))))
  nil)

(-> test-check-worker-temporary-cleanup () null)
(defun test-check-worker-temporary-cleanup ()
  "Test parent-owned cleanup after worker success, crash, timeout and unwind."
  (with-test-fixture (':posix-shell "check workers replaced by shell commands")
    (test-check--worker-temporary-cleanup))
  nil)

(-> test-check--worker-temporary-cleanup () null)
(defun test-check--worker-temporary-cleanup ()
  "Replace check workers with shell commands and verify parent-owned cleanup."
  (with-test-configuration (configuration root)
    (declare (ignore configuration))
    (test-check--call "CHECK--PARSE-ARGUMENTS" '("--jobs" "1"))
    (let ((runner (symbol-function (find-symbol "CHECK--RUN-PROCESSES" '#:cl-user)))
          (unrelated (test-make-temporary-root)))
      (unwind-protect
           (dolist (mode '(:normal :failure :crash :timeout :invalid :unwind))
             (let ((owned nil)
                   (entry nil)
                   (directory (merge-pathnames (format nil "~(~A~)/" mode) root)))
               (test-call-with-function-replacements
                (list
                 (list (find-symbol "CHECK--RUN-PROCESSES" '#:cl-user)
                       (lambda (entries &rest arguments)
                         (setf entry (first entries)
                               owned (getf (test-check--call
                                            "CHECK--READ-SINGLE-FORM"
                                            (fourth (test-check--call "CHECK-PROCESS-COMMAND" entry)))
                                           ':temporary-root))
                         (snapshot-write (merge-pathnames "unclaimed/state.sexp" owned)
                                         '(:fixture :present))
                         (when (eq mode :unwind)
                           (throw 'worker-exit :escaped))
                         (when (member mode '(:normal :invalid))
                           (test-check--call
                            "CHECK--WRITE-FORM"
                            (test-check--call "CHECK-PROCESS-RESULT-PATH" entry)
                            (when (eq mode :normal)
                              '(:version 1 :cases 1 :checks 1 :failures nil
                                :timings (("fixture-probe" 0d0))))))
                         (funcall (fdefinition '(setf cl-user::check-process-command))
                                  (list "sh" "-c"
                                        (case mode
                                          (:failure "exit 7")
                                          (:crash "kill -KILL $$")
                                          (:timeout "sleep 30 & wait")
                                          (otherwise "exit 0")))
                                  entry)
                         (apply runner entries arguments))))
                (lambda ()
                  (let ((*standard-output* (make-broadcast-stream))
                        (*error-output* (make-broadcast-stream)))
                    (test-assert
                     (eql (catch 'worker-exit
                            (test-check--call "CHECK--RUN-WORKERS" '(fixture-probe)
                                              :source-root (asdf:system-source-directory :autolith)
                                              :temporary-root directory :jobs 1 :timeout 1))
                          (case mode
                            (:normal t)
                            (:unwind :escaped)
                            (otherwise nil)))
                     "worker success, process failures and unwind preserve their outcomes"))))
               (test-assert (and owned (not (probe-file owned)))
                            "the parent deletes fixtures a dead worker cannot clean up")
               (test-assert (probe-file unrelated)
                            "worker cleanup preserves other runs' directories")
               (unless (eq mode :unwind)
                 (test-assert
                  (not (uiop:process-alive-p (test-check--call "CHECK-PROCESS-PROCESS" entry)))
                  "workers terminate before parent cleanup finishes"))))
        (platform-delete-directory-tree *platform* unrelated :validate t :if-does-not-exist ':ignore))))
  nil)
