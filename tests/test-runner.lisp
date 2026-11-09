(in-package #:autolith)

;;;; -- FiveAM Suite Registration --

(defvar *test-suites* nil
  "The ordered CLI suite names and their FiveAM test symbols.")

(define-condition test-selection-error (error)
  ((selector
    :initarg       :selector
    :reader        test-selection-error-selector
    :documentation "The unknown suite or test selector."))
  (:report (lambda (condition stream)
             (format stream "Unknown test selector ~S. Use --list to see available tests."
                     (test-selection-error-selector condition))))
  (:documentation "A test selection names no registered suite or case."))

(defmacro define-test-suite (name &body cases)
  "Register NAME and CASES in FiveAM and the command-line catalog.

Each case is an existing zero-argument function, invoked inside a FiveAM test.
Reloading a suite replaces its registration without duplicating cases."
  (let ((suite (intern (format nil "TEST-SUITE-~A" name) '#:autolith))
        (label (string-downcase name)))
    `(progn
       (fiveam:def-suite ,suite)
       ,@(loop for name in cases
               collect `(fiveam:test (,name :suite ,suite :compile-at :definition-time)
                          (let ((*tests-running-p* t)
                                (*telemetry-process-function*
                                  (lambda (run request) (declare (ignore run request)) nil))
                                (*model-catalog-fetch-function*
                                  (lambda (context) (declare (ignore context)) nil)))
                            (with-test-environment (("AUTOLITH_TELEMETRY" "off"))
                              (unwind-protect
                                   (,name)
                                (task-tests--close-orchestrators))))))
       (setf *test-suites*
             (append (remove ,label *test-suites* :key #'first :test #'string=)
                     (list (cons ,label ',cases)))))))


;;;; -- Selection and Execution --

(-> tests-select (&key (:suites list) (:tests list)) list)
(defun tests-select (&key suites tests)
  "Return unique cases selected by the union of exact SUITES and TESTS.

Names are case-insensitive. No selectors means the complete catalog. Unknown
selectors signal TEST-SELECTION-ERROR instead of silently running nothing."
  (let* ((all      (loop for suite in *test-suites* append (rest suite)))
         (selected nil))
    (dolist (selector suites)
      (let ((entry (assoc (string selector) *test-suites* :test #'string-equal)))
        (unless entry
          (error 'test-selection-error :selector selector))
        (setf selected (append selected (rest entry)))))
    (dolist (selector tests)
      (let ((name (find (string selector) all :key #'symbol-name
                                            :test  #'string-equal)))
        (unless name
          (error 'test-selection-error :selector selector))
        (push name selected)))
    (if (or suites tests)
        (remove-if-not (lambda (name) (member name selected)) all)
        all)))

(-> tests-list (&key (:suites list) (:tests list) (:stream stream)) null)
(defun tests-list (&key suites tests (stream *standard-output*))
  "Print selected suites and case names without executing tests."
  (let ((selected (tests-select :suites suites :tests tests)))
    (dolist (suite *test-suites*)
      (let ((cases (intersection (rest suite) selected)))
        (when cases
          (format stream "~&~A~%" (first suite))
          (dolist (name (rest suite))
            (when (member name cases)
              (format stream "  ~(~A~)~%" name)))))))
  nil)

(-> tests-run-cases (list &key (:stream stream) (:temporary-root (or null pathname))) list)
(defun tests-run-cases (cases &key (stream *standard-output*) temporary-root)
  "Run CASES sequentially and return a portable versioned result plist.

FiveAM catches case failures and continues. Timings are real seconds per case.
The caller owns process isolation; tests may mutate global functions and the
process environment. Delete every configuration fixture on exit, taking
ownership of TEMPORARY-ROOT when supplied or allocating a fresh run directory."
  (test-call-with-temporary-root
   (lambda (root)
     (declare (ignore root))
     (let ((checks                    0)
           (failures                  nil)
           (timings                   nil)
           (fiveam:*print-names*      nil)
           (fiveam:*on-error*         nil)
           (fiveam:*on-failure*       nil)
           (fiveam:*debug-on-error*   nil)
           (fiveam:*debug-on-failure* nil)
           (fiveam:*test-dribble*     (make-broadcast-stream)))
       (dolist (name cases)
         (unless (member name (tests-select))
           (error 'test-selection-error :selector name))
         (format stream "~&RUN ~(~A~)~%" name)
         (finish-output stream)
         (let* ((start   (get-internal-real-time))
                (results (fiveam:run name))
                (elapsed (/ (- (get-internal-real-time) start)
                            (float internal-time-units-per-second 1d0)))
                (passed  (fiveam:results-status results)))
           (incf checks (length results))
           (push (list (string-downcase name) elapsed) timings)
           (unless passed
             (let ((failure (list :test   (string-downcase name)
                                  :detail (with-output-to-string (output)
                                            (let ((fiveam:*test-dribble* output))
                                              (fiveam:explain! results))))))
               (format stream "~&~A:~%~A~%"
                       (getf failure :test) (getf failure :detail))
               (finish-output stream)
               (push failure failures)))
           (format stream "~&~A ~,3Fs ~(~A~)~%"
                   (if passed "PASS" "FAIL") elapsed name)
           (finish-output stream)))
       (list :version 1 :cases (length cases) :checks checks
             :failures (nreverse failures) :timings (nreverse timings))))
   :temporary-root temporary-root))

(-> tests-report (list &key (:stream stream) (:case-timings-p boolean)) boolean)
(defun tests-report (result &key (stream *standard-output*) (case-timings-p t))
  "Print RESULT's failures, totals and slowest cases; return success.

Include each case's timing unless CASE-TIMINGS-P is NIL."
  (when case-timings-p
    (dolist (timing (getf result ':timings))
      (format stream "~&~A ~,3Fs ~A~%"
              (if (find (first timing) (getf result ':failures)
                        :key  (lambda (failure) (getf failure ':test))
                        :test #'string=)
                  "FAIL" "PASS")
              (second timing) (first timing))))
  (dolist (failure (getf result ':failures))
    (format stream "~&~A:~%~A~%" (getf failure ':test) (getf failure ':detail)))
  (format stream "~&~:D cases, ~:D checks, ~D failed cases.~%"
          (getf result ':cases) (getf result ':checks)
          (length (getf result ':failures)))
  (format stream "Slowest cases:~%")
  (loop for (name seconds) in (sort (copy-list (getf result ':timings))
                                  #'> :key #'second)
        repeat 10
        do (format stream "  ~7,3Fs ~A~%" seconds name))
  (null (getf result ':failures)))

(-> run-tests (&key (:suites list) (:tests list)) boolean)
(defun run-tests (&key suites tests)
  "Run selected FiveAM tests in this image, signaling if any case fails.

Use script/check --jobs N for process-isolated parallel execution."
  (let ((result (tests-run-cases (tests-select :suites suites :tests tests))))
    (unless (tests-report result :case-timings-p nil)
      (error "Autolith tests failed."))
    t))
