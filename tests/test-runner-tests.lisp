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
  (let ((*test-suites*     '(("runner-probe" runner-probe-failure
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
           (let* ((result   (tests-run-cases (tests-select)
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
             (test-assert (not (tests-report result :stream (make-broadcast-stream)))
                         "failed cases produce a false report status"))
           (let* ((root    (test-make-temporary-root))
                  (output  (make-string-output-stream))
                  (cleanup (make-condition 'test-fixture-error)))
             (unwind-protect
                  (let ((observed
                          (handler-case
                              (test-call-with-function-replacements
                               (list (list 'platform-delete-directory-tree
                                           (lambda (&rest arguments)
                                             (declare (ignore arguments))
                                             (error cleanup))))
                               (lambda ()
                                 (tests-run-cases '(runner-probe-failure)
                                                  :stream output :temporary-root root)))
                            (test-fixture-error (condition) condition))))
                    (test-assert (eq cleanup observed)
                                 "fixture cleanup failure escapes without replacement")
                    (test-assert (search "intentional failed assertion"
                                         (get-output-stream-string output))
                                 "primary case evidence is emitted before failed cleanup"))
               (platform-delete-directory-tree *platform* root
                                               :validate t :if-does-not-exist ':ignore))))
      (dolist (name '(runner-probe-failure runner-probe-error runner-probe-success))
        (fiveam:rem-test name))))
  nil)

(-> test-runner-temporary-cleanup () null)
(defun test-runner-temporary-cleanup ()
  "Test cleanup around successful, failed and nonlocally exiting FiveAM cases."
  (let* ((*test-suites* '(("cleanup-probe" runner-cleanup-probe)))
         (fixture-root  nil)
         (run-root      nil)
         (exit          nil)
         (previous      (sb-ext:symbol-global-value '*test-temporary-root*))
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

(-> test-check-committed-version () null)
(defun test-check-committed-version ()
  "Read recovery expectations from committed VERSION rather than dirty source."
  (with-test-configuration (configuration root)
    (declare (ignore configuration))
    (let* ((checkout     (merge-pathnames "checkout/" root))
           (version-file (merge-pathnames "VERSION" checkout)))
      (ensure-directories-exist version-file)
      (with-test-environment
          (("GIT_CONFIG_NOSYSTEM" "1")
           ("GIT_CONFIG_GLOBAL" (namestring (merge-pathnames "no-global-config" root)))
           ("GIT_CONFIG_COUNT" "0") ("GIT_CONFIG_PARAMETERS" ""))
        (flet ((git (&rest arguments)
                 "Run a Git fixture command without signing or hooks."
                 (uiop:run-program
                  (append (list "git" "-C" (namestring checkout)
                                "-c" "user.name=Autolith Test"
                                "-c" "user.email=test@invalid"
                                "-c" "core.hooksPath=")
                          arguments)
                  :output ':string :error-output ':string))
               (write-version (version)
                 "Replace the working version file."
                 (with-open-file (stream version-file :direction ':output
                                         :if-exists              ':supersede
                                         :if-does-not-exist      ':create)
                   (write-line version stream))))
          (git "init" "--quiet")
          (dolist (version '("0.65.0" "0.65.1"))
            (write-version version)
            (git "add" "VERSION")
            (git "commit" "--quiet" "--no-gpg-sign" "-m" "Set fixture version")
            (write-version "9.9.9")
            (test-assert
             (string= version (test-check--call "CHECK--COMMITTED-VERSION" checkout))
             "recovery expectations use each committed VERSION despite working edits"))))))
  nil)

(-> test-check-load-test-system-lock () null)
(defun test-check-load-test-system-lock ()
  "Test cache-scoped loader serialization, callback failure, and checkout fallback."
  (dolist (cache-p '(nil t))
    (with-test-configuration (configuration root)
      (declare (ignore configuration))
      (let* ((source-root     (merge-pathnames "checkout/" root))
             (cache-root      (when cache-p (merge-pathnames "asdf-cache/" root)))
             (lock-path       (merge-pathnames "test-load.lock"
                                               (or cache-root
                                                   (merge-pathnames ".qlot/" source-root))))
             (state-lock      (make-lock "check test-system loader state"))
             (state-condition (make-condition-variable))
             (first-entered-p nil)
             (release-first-p nil)
             (active          0)
             (maximum-active  0)
             (calls           0)
             (errors          nil)
             (threads         nil))
        (ensure-directories-exist lock-path)
        (with-test-environment (("AUTOLITH_ASDF_CACHE"
                                 (when cache-root (uiop:native-namestring cache-root))))
          (unwind-protect
               (test-call-with-function-replacements
                (list
                 (list 'asdf:load-system
                       (lambda (system &rest arguments)
                         (declare (ignore arguments))
                         (when (eq system :autolith/tests)
                           (with-lock-held (state-lock)
                             (incf calls)
                             (incf active)
                             (setf maximum-active (max maximum-active active))
                             (let ((first-p (= calls 1)))
                               (when first-p
                                 (setf first-entered-p t)
                                 (condition-notify state-condition))
                               (unwind-protect
                                    (when first-p
                                      (loop until release-first-p
                                            do (unless (condition-wait state-condition state-lock
                                                                       :timeout 10)
                                                 (error 'test-fixture-error))))
                                 (decf active))
                               (when first-p
                                 (error 'test-fixture-error))))))))
                (lambda ()
                  (labels ((load-test-system ()
                             (handler-case
                                 (test-check--call "CHECK--LOAD-TEST-SYSTEM" source-root)
                               (error (condition)
                                 (with-lock-held (state-lock)
                                   (push condition errors)))))

                           (join-bounded (thread)
                             (loop repeat 1000 while (thread-alive-p thread)
                                   do (sleep 0.01))
                             (when (thread-alive-p thread)
                               (destroy-thread thread))
                             (join-thread thread)))
                    (setf threads
                          (list (make-thread #'load-test-system :name "check loader one")))
                    (with-lock-held (state-lock)
                      (loop until first-entered-p
                            do (unless (condition-wait state-condition state-lock
                                                       :timeout 10)
                                 (error 'test-fixture-error))))
                    (push (make-thread #'load-test-system :name "check loader two") threads)
                    (sleep 0.05)
                    (with-lock-held (state-lock)
                      (setf release-first-p t)
                      (condition-notify state-condition))
                    (dolist (thread threads)
                      (join-bounded thread))
                    (test-assert (= maximum-active 1)
                                 "concurrent test-system callbacks never overlap")
                    (test-assert (= calls 2)
                                 "both concurrent loaders reach the mocked test-system load")
                    (test-assert (= (length errors) 1)
                                 "the first callback failure propagates without losing the lock")
                    (test-assert (typep (first errors) 'test-fixture-error)
                                 "callback conditions propagate through the loader helper")
                    (test-assert (probe-file lock-path)
                                 "the loader creates its lock in the shared cache directory")
                    (when cache-p
                      (test-assert (not (probe-file source-root))
                                   "an explicit cache requires no writes to the source checkout")))))
            (with-lock-held (state-lock)
              (setf release-first-p t)
              (condition-notify state-condition))
            (dolist (thread threads)
              (when (thread-alive-p thread)
                (loop repeat 1000 while (thread-alive-p thread)
                      do (sleep 0.01))
                (when (thread-alive-p thread)
                  (destroy-thread thread)))
              (join-thread thread)))))))
  nil)

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
    (let* ((cases    '(a b c d e))
           (shards   (test-check--call "CHECK--PARTITION-CASES" cases jobs))
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

(-> test-check-worker-image () null)
(defun test-check-worker-image ()
  "Verify the CLI executes cases from a source-matched preloaded test image."
  (let ((core (uiop:getenv "AUTOLITH_TEST_WORKER_CORE")))
    (if core
        (let ((source-root (asdf:system-source-directory :autolith)))
          (test-assert
           (and sb-ext:*core-pathname*
                (equal (platform-truename *platform* (uiop:parse-native-namestring core))
                       (platform-truename *platform* sb-ext:*core-pathname*)))
           "The case runs in the test image supplied by the CLI.")
          (test-assert
           (sbcl-generations:image-build-record-compatible-p
            (sbcl-generations:image-running-build-record) source-root
            :inputs (lambda (root) (test-check--call "TEST-WORKER-SOURCE-PATHS" root)))
           "The running image records every application and test input."))
        (fiveam:skip "Run this image-lifecycle check through script/check.")))
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
    (let* ((marker-a    (merge-pathnames "a" root))
           (marker-b    (merge-pathnames "b" root))
           (late-marker (merge-pathnames "leaked" root))
           (entries
             (loop for own in (list marker-a marker-b)
                   for peer in (list marker-b marker-a)
                   for index from 1
                   collect
                   (test-check--call
                    "MAKE-CHECK-PROCESS"
                    :label   "concurrent barrier"
                    :command (list "sh" "-c"
                                   (format nil "touch ~A; while [ ! -e ~A ]; do sleep 0.02; done"
                                           (uiop:escape-shell-token (namestring own))
                                           (uiop:escape-shell-token (namestring peer))))
                    :output  (merge-pathnames (format nil "barrier-~D.log" index) root)))))
      (test-check--call "CHECK--RUN-PROCESSES" entries :jobs 2 :timeout 5)
      (test-assert
       (every (lambda (entry)
                (and (eql 0 (test-check--call "CHECK-PROCESS-STATUS" entry))
                     (null (test-check--call "CHECK-PROCESS-PROBLEM" entry))))
              entries)
       "workers reach a mutual barrier concurrently")
      (let ((failure (test-check--call "MAKE-CHECK-PROCESS" :label "failed command"
                                      :command                     '("sh" "-c" "exit 7")
                                      :output                      (merge-pathnames "failure.log" root)))
            (timeout (test-check--call
                      "MAKE-CHECK-PROCESS" :label "deadline"
                      :command                    (list "sh" "-c"
                                                        (format nil "(sleep 2; touch ~A) & wait"
                                                                (uiop:escape-shell-token
                                                                 (namestring late-marker))))
                      :output                     (merge-pathnames "timeout.log" root))))
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
    (let ((runner    (symbol-function (find-symbol "CHECK--RUN-PROCESSES" '#:cl-user)))
          (unrelated (test-make-temporary-root)))
      (unwind-protect
           (dolist (mode '(:normal :failure :crash :timeout :invalid :incomplete :unwind))
             (let ((owned     nil)
                   (entry     nil)
                   (output    (make-string-output-stream))
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
                         (when (eq mode ':normal)
                           (let ((environment (test-check--call "CHECK-PROCESS-ENVIRONMENT" entry))
                                 (worker-root (merge-pathnames "worker-1/" directory)))
                             (dolist (entry '(("XDG_CONFIG_HOME" "config/")
                                              ("XDG_STATE_HOME" "state/")
                                              ("XDG_CACHE_HOME" "cache/")))
                               (test-assert
                                (member (format nil "~A=~A" (first entry)
                                                (merge-pathnames (second entry) worker-root))
                                        environment :test #'string=)
                                "Default-root configurations use worker-owned configuration, state and caches."))))
                         (snapshot-write (merge-pathnames "unclaimed/state.sexp" owned)
                                         '(:fixture :present))
                         (when (eq mode :unwind)
                           (throw 'worker-exit :escaped))
                         (when (member mode '(:normal :invalid :incomplete))
                           (test-check--call
                            "CHECK--WRITE-FORM"
                            (test-check--call "CHECK-PROCESS-RESULT-PATH" entry)
                            (when (member mode '(:normal :incomplete))
                              '(:version 1 :cases 1 :checks 1 :failures nil
                                :timings (("fixture-probe" 0d0))))))
                         (funcall (fdefinition '(setf cl-user::check-process-command))
                                  (list "sh" "-c"
                                        (case mode
                                          (:failure
                                           "exit 7")
                                          (:crash
                                           "kill -KILL $$")
                                          (:timeout
                                           "sleep 30 & wait")
                                          (otherwise
                                           "exit 0")))
                                  entry)
                         (apply runner entries arguments))))
                (lambda ()
                  (let ((*standard-output* output)
                        (*error-output*    output))
                    (test-assert
                     (eql (catch 'worker-exit
                             (test-check--call "CHECK--RUN-WORKERS"
                                               (if (eq mode ':incomplete)
                                                   '(fixture-probe missing-probe)
                                                   '(fixture-probe))
                                              :source-root (asdf:system-source-directory :autolith)
                                              :temporary-root directory :jobs 1 :timeout 1))
                          (case mode
                            (:normal
                             t)
                            (:unwind
                             :escaped)
                            (otherwise
                             nil)))
                     "worker success, process failures and unwind preserve their outcomes"))))
               (when (eq mode ':incomplete)
                 (let ((diagnostic (get-output-stream-string output)))
                   (test-assert (and (search "fixture-probe" diagnostic)
                                     (search "missing-probe" diagnostic))
                                "unvalidated partial shard reports every missing assigned case")))
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
