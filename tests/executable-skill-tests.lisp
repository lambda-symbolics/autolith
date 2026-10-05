(in-package #:autolith)

;;;; -- Executable Skill Product Tests --

(-> executable-skill-tests--write (pathname string) pathname)
(defun executable-skill-tests--write (pathname content)
  "Write an isolated UTF-8 executable fixture."
  (ensure-directories-exist pathname)
  (with-open-file (stream pathname :direction ':output :if-exists ':supersede
                          :if-does-not-exist ':create :external-format ':utf-8)
    (write-string content stream))
  pathname)

(-> executable-skill-tests--manifest (&key (:output list) (:version string)) string)
(defun executable-skill-tests--manifest (&key (output '(:type :object)) (version "1.0.0"))
  "Render valid native executable metadata for the disposable fixture."
  (with-standard-io-syntax
    (write-to-string
     `(:skill-executable :format-version 1 :skill-version ,version
       :system "executable-skill-fixture" :system-version "1.0.0"
       :entrypoint ("AUTOLITH" "EXECUTABLE-SKILL-FIXTURE-RUN")
       :self-test ("AUTOLITH" "EXECUTABLE-SKILL-FIXTURE-SELF-TEST")
       :verify ("AUTOLITH" "EXECUTABLE-SKILL-FIXTURE-VERIFY")
       :capabilities ("workspace-read") :tools ("resource.read" "search.files")
       :input (:type :object) :output ,output))))

(-> executable-skill-tests--fixture (function) null)
(defun executable-skill-tests--fixture (function)
  "Provide real registry, command authorizer and executable workflow source."
  (let* ((configuration (test-configuration))
         (root (test-configuration-root configuration))
         (configuration (configuration-copy configuration :working-directory root))
         (directory (merge-pathnames "executable-fixture/" (skill-global-root configuration)))
         (marker (merge-pathnames "loaded.marker" directory))
         (sidecar (merge-pathnames "EXECUTABLE.sexp" directory))
         (registry (make-default-tool-registry :configuration configuration))
         (conversation (conversation-create configuration))
         (pool (lisp-worker-pool-create configuration))
         (agent (make-instance 'agent :configuration configuration :conversation conversation
                                :worker pool :tool-registry registry))
         (decision ':full-access)
         (admissions nil)
         (context (make-instance 'tool-context :configuration configuration :conversation conversation
                                  :worker pool :registry registry :agent agent :call-id "skill-origin"
                                  :command-authorization-function
                                  (lambda (command directory)
                                    (declare (ignore directory))
                                    (push command admissions)
                                    (if (functionp decision) (funcall decision command) decision))))
         (asd (merge-pathnames "executable-skill-fixture.asd" directory)))
    (unwind-protect
         (progn
           (executable-skill-tests--write
            (merge-pathnames "SKILL.sexp" directory)
            "(:autolith-skill :version 1 :name \"executable-fixture\" :description \"Executable transport fixture\" :instructions \"Loading prose grants no execution authority.\")")
           (executable-skill-tests--write sidecar (executable-skill-tests--manifest))
           (executable-skill-tests--write
            asd "(asdf:defsystem \"executable-skill-fixture\" :version \"1.0.0\" :components ((:file \"workflow\")))")
           (executable-skill-tests--write
            (merge-pathnames "workflow.lisp" directory)
            (format nil
                    "(in-package #:autolith)~%
(with-open-file (stream ~S :direction :output :if-exists :supersede :if-does-not-exist :create) (write-string \"loaded\" stream))~%
(defparameter *executable-skill-fixture-count* 0)~%
(defun executable-skill-fixture-run (input &key context)~%
  (declare (ignore context))~%
  (when (second (assoc \"large\" (rest input) :test #'string=))~%
    (return-from executable-skill-fixture-run~%
      (list :object (list \"large\" (make-string 16000 :initial-element #\\x))~%
            (list \"deep\" (loop repeat 16 for value = \"leaf\" then (list :array value) finally (return value))))))~%
  (let ((read-result (autolith:worker-tool-call \"resource.read\" \"{\\\"uri\\\":\\\"workspace:payload.txt\\\"}\"))~%
        (search-result (autolith:worker-tool-call \"search.files\" \"{\\\"query\\\":\\\"payload\\\"}\"))~%
        (denied (autolith:worker-tool-call \"self.status\" \"{}\")))~%
    (list :object (list \"read\" (getf read-result :content))~%
          (list \"search\" (getf search-result :success-p))~%
          (list \"search-content\" (getf search-result :content))~%
          (list \"denied\" (eq (getf denied :code) :capability-denied))~%
          (list \"counter\" (incf *executable-skill-fixture-count*)))))~%
(defun executable-skill-fixture-self-test (&key context)~%
  (declare (ignore context))~%
  (getf (autolith:worker-tool-call \"resource.read\" \"{\\\"uri\\\":\\\"workspace:payload.txt\\\"}\") :success-p))~%
(defun executable-skill-fixture-verify (&key context) (declare (ignore context)) nil)~%"
                    (namestring marker)))
           (executable-skill-tests--write
            (merge-pathnames "payload.txt" (config :working-directory configuration)) "skill-payload")
           (funcall function context asd sidecar marker
                    (lambda (value) (setf decision value))
                    (lambda () (copy-list admissions))))
      (unwind-protect
           (tool-registry-close-runtime-state registry)
        (lisp-worker-pool-stop-all pool)
        (platform-delete-directory-tree *platform* root :validate t :if-does-not-exist ':ignore))))
  nil)

(-> executable-skill-tests--call (tool-context string &key (:asd pathname) (:input t)
                                (:entrypoint (option string))) tool-result)
(defun executable-skill-tests--call (context operation &key asd (input (json-object)) entrypoint)
  "Use ordinary model-facing registry dispatch for a fixture operation."
  (let ((arguments (json-object "name" "executable-fixture" "asd" (namestring asd))))
    (when (string= operation "invoke") (setf (gethash "input" arguments) input))
    (when entrypoint (setf (gethash "entrypoint" arguments) entrypoint))
    (tool-registry-execute-call
     (tool-context-registry context)
     (json-object "namespace" "skill" "name" operation "call_id" "skill-origin"
                  "arguments" (json-encode arguments)) context)))

(-> test-executable-skill-admission () null)
(defun test-executable-skill-admission ()
  "Metadata and prose selection confer no code or exact host-tool authority."
  (executable-skill-tests--fixture
   (lambda (context asd sidecar marker set-decision admissions)
     (declare (ignore sidecar))
     (let ((configuration (tool-context-configuration context)))
       (test-assert (executable-skill-discover configuration "executable-fixture") "executable metadata discovers")
       (let ((*skill-logical-turn-state* (make-instance 'skill-logical-turn-state)))
         (skill-select-for-logical-turn configuration "executable-fixture"))
       (test-assert (not (probe-file marker)) "discovery and prose selection execute nothing")
       (funcall set-decision ':deny)
       (let ((result (executable-skill-tests--call context "invoke" :asd asd)))
         (test-assert (not (tool-result-success-p result)) "explicit denial rejects executable")
         (test-assert (eq (tool-result-error-code result) ':not-authorized) "typed denial")
         (test-assert (not (probe-file marker)) "denial occurs before ASD and code loading"))
       (test-assert (= 1 (length (funcall admissions))) "one exact code admission")
       (funcall set-decision ':sandboxed)
       (let ((result (executable-skill-tests--call context "invoke" :asd asd)))
         (test-assert (eq (tool-result-error-code result) ':not-authorized) "sandbox-only approval cannot load executable code")
         (test-assert (not (probe-file marker)) "sandbox-only rejection precedes code load"))
       (funcall set-decision ':full-access)
       (let ((*worker-host-tool-policy* (list :restricted-p t :allowlist '("skill.invoke"))))
         (let ((result (executable-skill-tests--call context "invoke" :asd asd)))
           (test-assert (eq (tool-result-error-code result) ':capability-denied) "originating exact policy restricts Skill callbacks")
           (test-assert (not (probe-file marker)) "restricted denial precedes code load")))))))

(-> test-executable-skill-workflow () null)
(defun test-executable-skill-workflow ()
  "Compose actual resource/search calls with isolated state and correlated audit."
  (executable-skill-tests--fixture
   (lambda (context asd sidecar marker set-decision admissions)
     (declare (ignore admissions))
     (dotimes (index 2)
       (declare (ignorable index))
       (let ((result (executable-skill-tests--call context "invoke" :asd asd)))
         (test-assert (tool-result-success-p result) (format nil "workflow runs: ~A" (tool-result-content result)))
         (when (tool-result-success-p result)
           (let* ((record (executable-skill-result-record result))
                  (value (getf record :value))
                  (identity (getf record :identity)))
             (test-assert (search "skill-payload" (second (assoc "read" (rest value) :test #'string=))) "actual resource read")
             (test-assert (second (assoc "search" (rest value) :test #'string=))
                          (format nil "actual search operation: ~A"
                                  (second (assoc "search-content" (rest value) :test #'string=))))
             (test-assert (second (assoc "denied" (rest value) :test #'string=)) "undeclared host tool rejected")
             (test-assert (= 1 (second (assoc "counter" (rest value) :test #'string=))) "every invocation starts fresh heap")
             (test-assert (equal (getf identity :skill-version) "1.0.0") "native transport retains version")
             (test-assert (= 64 (length (getf identity :digest))) "native transport retains digest")))))
     (test-assert (probe-file marker) "worker loaded fixture after authorization")
     (test-assert (not (fboundp 'executable-skill-fixture-run)) "fixture never loads into active test image")
     (let ((calls nil))
       (conversation-map-records
        (tool-context-conversation context)
        (lambda (record) (when (eq (first record) ':worker-tool-call) (push record calls))))
       (test-assert (>= (length calls) 6) "callback audit records workflow calls")
       (test-assert (every (lambda (record) (equal (getf (rest record) :parent-call-id) "skill-origin")) calls)
                    "callback audit retains parent identity"))
     (let* ((pathname (merge-pathnames "SKILL.sexp" (uiop:pathname-directory-pathname sidecar)))
            (source (uiop:read-file-string pathname))
            (manifest (uiop:read-file-string sidecar)))
       (dolist (mutation '( :sidecar :name :removed))
         (when (probe-file marker) (delete-file marker))
         (funcall set-decision
                  (lambda (command)
                    (when (search "skill.execute --" command)
                      (case mutation
                        (:sidecar
                         (executable-skill-tests--write sidecar (executable-skill-tests--manifest :version "2.0.0")))
                        (:name
                         (executable-skill-tests--write pathname
                          "(:autolith-skill :version 1 :name \"renamed\" :description \"Changed\" :instructions \"Changed\")"))
                        (:removed
                         (delete-file pathname))))
                    ':full-access))
         (unwind-protect
              (let ((result (executable-skill-tests--call context "invoke" :asd asd)))
                (test-assert (eq ':identity-changed (tool-result-error-code result))
                             (format nil "post-admission ~A mutation rejected: ~A" mutation (tool-result-content result)))
                (test-assert (not (probe-file marker)) "identity rejection precedes code loading")
                (when (eq mutation ':removed)
                  (test-assert (getf (executable-skill-result-record result) :diagnostics)
                               "unavailable exact source retains structured diagnostics")))
           (executable-skill-tests--write pathname source)
           (executable-skill-tests--write sidecar manifest)))
       (funcall set-decision ':full-access)))))

(-> executable-skill-tests--transport (tool-context) null)
(defun executable-skill-tests--transport (context)
  "Reject unsafe envelopes and clean partial publications on job cancellation."
  (let* ((configuration (tool-context-configuration context))
         (directory (merge-pathnames "transport-fixture/" (config :cache-root configuration)))
         (pathname (merge-pathnames "result.sexp" directory)))
    (unwind-protect
         (progn
           (ensure-directories-exist pathname)
           (let ((status (platform-path-status *platform* directory :follow-links-p nil)))
             (dolist (source '("(:token \"wrong\" :record (:success-p t))"
                               "(:token \"fixture\" :record (:success-p t)) :extra"
                               "#.(error \"reader evaluation\")"
                               "(:token \"fixture\" :record #100000000(0))"
                               "(:token \"fixture\" :record #1=(#1#))"))
               (executable-skill-tests--write pathname source)
               (test-assert
                (handler-case
                    (progn (executable-skill--read-result pathname "fixture" :directory-status status) nil)
                  (error () t)) "unsafe or extra-form publication rejected"))
             (dolist (source (list
                             (concatenate 'string (make-string 140 :initial-element #\()
                                          "0" (make-string 140 :initial-element #\)))
                             (format nil "(:token ~S :record (~{~D ~}))"
                                     "fixture" (make-list 262145 :initial-element 0))))
               (executable-skill-tests--write pathname source)
               (test-assert
                (handler-case
                    (progn (executable-skill--read-result pathname "fixture" :directory-status status) nil)
                  (executable-skill-error () t)) "preconstruction depth and node bounds reject publication"))
             (let ((saved-directory (merge-pathnames "transport-original/" (config :cache-root configuration)))
                   (open-function (symbol-function 'platform-open-regular-file))
                   (opened-stream nil))
               (unwind-protect
                    (test-call-with-function-replacements
                     (list
                      (list 'platform-open-regular-file
                            (lambda (platform path &key follow-links-p)
                              (rename-file directory saved-directory)
                              (executable-skill-tests--write
                               path "(:token \"fixture\" :record (:success-p t :value :substituted))")
                              (multiple-value-bind (stream file-status)
                                  (funcall open-function platform path :follow-links-p follow-links-p)
                                (setf opened-stream stream)
                                (values stream file-status)))))
                     (lambda ()
                       (test-assert
                        (handler-case
                            (progn (executable-skill--read-result pathname "fixture" :directory-status status) nil)
                          (executable-skill-error () t))
                        "directory replacement between identity observation and file open rejected")))
                 (when (probe-file saved-directory)
                   (platform-delete-directory-tree *platform* directory :validate t :if-does-not-exist ':ignore)
                   (rename-file saved-directory directory)))
               (test-assert (and opened-stream (not (open-stream-p opened-stream)))
                            "rejected substituted file stream is closed"))
             (executable-skill-tests--write pathname "(:token \"fixture\" :record (:success-p t :value :null))")
             (let ((*executable-skill-result-byte-limit* 16))
               (test-assert
                (handler-case
                    (progn (executable-skill--read-result pathname "fixture" :directory-status status) nil)
                  (executable-skill-error () t)) "oversized publication rejected before reading"))
             (test-assert
              (handler-case
                  (progn
                    (executable-skill--read-result
                     pathname "fixture" :directory-status
                     (platform-path-status *platform* (config :cache-root configuration) :follow-links-p nil)) nil)
                (executable-skill-error () t)) "changed publication directory identity rejected")))
      (platform-delete-directory-tree *platform* directory :validate t :if-does-not-exist ':ignore)))
  (let ((worker nil) (directory nil))
    (test-call-with-function-replacements
     (list
      (list 'worker-host-eval-request
            (lambda (actual operation arguments &key context tools enabled-p)
              (declare (ignore operation arguments tools enabled-p))
              (setf worker actual
                    directory (merge-pathnames
                               (format nil "executable-results/~A/" (subseq (lisp-worker-name actual) 6))
                               (config :cache-root (tool-context-configuration context))))
              (executable-skill-tests--write (merge-pathnames "result.sexp" directory) "(:partial")
              (error 'job-aborted :identifier "fixture" :reason ':cancelled :message "Cancel fixture"))))
     (lambda ()
       (test-assert
        (handler-case (progn (executable-skill--run context "t" :tools nil) nil)
          (job-aborted () t)) "cancellation control condition propagates")))
    (test-assert (not (lisp-worker-running-p worker)) "cancelled Skill heap is stopped")
    (test-assert (not (probe-file directory)) (format nil "cancelled partial publication removed: ~S" directory)))
  nil)

(-> test-executable-skill-verification-contracts () null)
(defun test-executable-skill-verification-contracts ()
  "Exercise self-test, explicit verification, contracts and post-admission identity."
  (executable-skill-tests--fixture
   (lambda (context asd sidecar marker set-decision admissions)
     (declare (ignore marker set-decision admissions))
     (let ((result (executable-skill-tests--call context "verify" :asd asd)))
       (test-assert (tool-result-success-p result) (format nil "self-test runs through bridge: ~A" (tool-result-content result))))
     (let ((result (executable-skill-tests--call context "verify" :asd asd :entrypoint "verify")))
       (test-assert (eq (tool-result-error-code result) ':verification-failed) "verification requires exact T"))
     (let ((result (executable-skill-tests--call context "invoke" :asd asd :input "wrong")))
       (test-assert (eq (tool-result-error-code result) ':invalid-input) "native input contract enforced"))
     (executable-skill-tests--write sidecar (executable-skill-tests--manifest :output '(:type :string)))
     (let ((result (executable-skill-tests--call context "invoke" :asd asd)))
       (test-assert (eq (tool-result-error-code result) ':invalid-output) "portable output contract enforced"))
     (multiple-value-bind (executable metadata)
         (executable-skill-discover (tool-context-configuration context) "executable-fixture")
       (let ((form (executable-skill--form metadata executable :operation ':invoke :input '(:object) :asd asd)))
         (executable-skill-tests--write sidecar (executable-skill-tests--manifest :version "2.0.0"))
         (let ((result (executable-skill--run context form :tools '("resource.read" "search.files"))))
           (test-assert (eq (tool-result-error-code result) ':identity-changed) "changed sidecar rejected before definition loading"))))
     (executable-skill-tests--write sidecar (executable-skill-tests--manifest))
     (let ((result (executable-skill-tests--call context "invoke" :asd asd :input (json-object "large" t))))
       (test-assert (tool-result-success-p result) (format nil "large portable output runs: ~A" (tool-result-content result)))
       (when (tool-result-success-p result)
         (let* ((value (getf (executable-skill-result-record result) :value))
                (deep (second (assoc "deep" (rest value) :test #'string=))))
           (test-assert (= 16000 (length (second (assoc "large" (rest value) :test #'string=)))) "native transport preserves more than 12k characters")
           (dotimes (index 15) (declare (ignorable index)) (setf deep (second deep)))
           (test-assert (equal deep "leaf") "native transport preserves deep portable arrays"))))
     (executable-skill-tests--transport context))))
