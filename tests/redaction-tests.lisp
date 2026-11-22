(in-package #:autolith)

;;;; -- Public Local Redaction --

(-> test-redaction-lifecycle () null)
(defun test-redaction-lifecycle ()
  "Exercise public inference, path forwarding, language, and context cleanup."
  (let ((context (gensym "REDACTOR"))
        (created 0)
        (closed  0)
        (pieces  nil))
    (test-call-with-function-replacements
     (list (list 'defingerprinter:load-library (lambda (path) (declare (ignore path)) t))
           (list 'defingerprinter:make-redactor
                 (lambda (&key model-directory runtime-library)
                   (test-assert (equal model-directory #P"/model/") "Forward the model path.")
                   (test-assert (equal runtime-library #P"/runtime") "Forward the runtime path.")
                   (incf created)
                   context))
           (list 'defingerprinter:redact
                 (lambda (redactor piece &key language)
                   (test-assert (eq redactor context) "Use the call's context for every piece.")
                   (test-assert (equal language "de") "Forward the selected language.")
                   (push piece pieces)
                   (values piece ':ok)))
           (list 'defingerprinter:close-redactor
                 (lambda (redactor)
                   (test-assert (eq redactor context) "Close the acquired context.")
                   (incf closed))))
     (lambda ()
       (dolist (text (list "" "Short text."
                           (apply #'concatenate 'string
                                  (make-list 200 :initial-element "Check the result. "))))
         (multiple-value-bind (safe status)
             (redact text :language "de" :model-directory #P"/model/" :runtime-library #P"/runtime")
           (test-assert (and (equal safe text) (eq status ':ok)) "Return all text and success.")))
       (test-assert (= created closed 3) "Release every context, including empty input.")
       (test-assert (> (length pieces) 2) "Split long text into bounded pieces."))))
  nil)

(-> test-redaction-failures () null)
(defun test-redaction-failures ()
  "Reject partial or saturated output and release contexts on every failure path."
  (let ((closed 0)
        (calls  0)
        (mode   ':ok))
    (test-call-with-function-replacements
     (list (list 'defingerprinter:load-library (lambda (path) (declare (ignore path)) t))
           (list 'defingerprinter:make-redactor
                 (lambda (&key model-directory runtime-library)
                   (declare (ignore model-directory runtime-library))
                   (when (eq mode ':setup)
                     (error 'defingerprinter:setup-error :status ':artifact-error))
                   ':context))
           (list 'defingerprinter:redact
                 (lambda (context text &key language)
                   (declare (ignore context language))
                   (incf calls)
                   (case mode
                     (:saturated
                      (values (make-string 1024 :initial-element #\x) ':ok))
                     (:invalid-output
                      (values nil ':ok))
                     (:later-failure
                      (if (= calls 2)
                          (values nil ':inference-error)
                          (values text ':ok)))
                     (otherwise
                      (values text ':ok)))))
           (list 'defingerprinter:close-redactor
                 (lambda (context) (declare (ignore context)) (incf closed))))
     (lambda ()
       (dolist (entry '((:saturated :output-limit)
                        (:invalid-output :output-limit)
                        (:later-failure :inference-error)
                        (:setup :artifact-error)
                        (:ok :unsupported-input)))
         (setf mode (first entry) calls 0)
         (let ((text (if (eq mode ':ok)
                         (make-string 513 :initial-element #\x)
                         (apply #'concatenate 'string
                                (make-list 100 :initial-element "Check the result. ")))))
           (multiple-value-bind (safe status) (redact text)
             (test-assert (null safe) "Return no text after failure.")
             (test-assert (eq status (second entry)) "Return the failure status."))))
       (test-assert (= closed 4) "Close acquired contexts, but not failed acquisitions."))))
  nil)

(-> test-redaction-scratchpad () null)
(defun test-redaction-scratchpad ()
  "Call the exported function from a fresh scratchpad worker without model setup."
  (with-test-configuration (configuration root)
    (declare (ignore root))
    (let* ((pool     (lisp-worker-pool-create configuration))
           (registry (make-default-tool-registry))
           (context  (make-instance 'tool-context
                                   :configuration configuration :worker pool
                                   :conversation (conversation-create configuration)))
           (path     (merge-pathnames "redact.lisp" (lisp-scratchpad-root context))))
      (unwind-protect
           (progn
             (ensure-directories-exist path)
             (with-open-file (stream path :direction ':output :if-exists ':error)
               (write-line
                "(defparameter *redaction-result* (multiple-value-list (autolith:redact \"Fixture\" :model-directory \"relative\" :runtime-library \"relative\")))"
                stream))
             (test-assert
              (tool-result-success-p
               (tool-execute (tool-registry-find registry "lisp" "scratchpad-run")
                             context (json-object "path" "redact.lisp" "repl" "redact")))
              "Read and call the exported function from a fresh worker.")
             (let ((result
                     (lisp-worker-request
                      (lisp-worker-manager-worker pool "redact") ':eval
                      '(:forms ("(and (null (first *redaction-result*)) (keywordp (second *redaction-result*)))")))))
               (test-assert (equal (getf (rest result) :values) '("T"))
                            "Return setup failure values through scratchpad execution.")))
        (lisp-worker-pool-stop-all pool))))
  nil)

(-> test-redaction-real-model () null)
(defun test-redaction-real-model ()
  "Opt in to local native inference with AUTOLITH_TEST_TELEMETRY_MODEL=1."
  (when (equal (uiop:getenv "AUTOLITH_TEST_TELEMETRY_MODEL") "1")
    (multiple-value-bind (safe status) (redact "Alice Smith lives in Amsterdam.")
      (test-assert (and (eq status ':ok) (stringp safe)) "Run the actual censorship model.")
      (test-assert (not (search "Alice Smith" safe)) "Redact the fixture name.")))
  nil)
