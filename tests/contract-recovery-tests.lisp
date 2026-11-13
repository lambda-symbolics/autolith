(in-package #:autolith)

;;;; -- Emergency Contract Recovery --

(-> contract-recovery-tests--octets (pathname) (simple-array (unsigned-byte 8) (*)))
(defun contract-recovery-tests--octets (pathname)
  "Read PATHNAME without text decoding for byte-exact recovery assertions."
  (with-open-file (stream pathname :element-type '(unsigned-byte 8))
    (let ((octets (make-array (file-length stream) :element-type '(unsigned-byte 8))))
      (read-sequence octets stream)
      octets)))

(-> test-contract-reset-unreadable () null)
(defun test-contract-reset-unreadable ()
  "Recover through ordinary dispatch without replaying malformed contract records."
  (with-test-configuration (configuration)
    (contract-tests--define-ready configuration)
    (knowledge-assert configuration '("report.ready" "retained") :basis ':assumption)
    (let* ((pathname     (configuration-knowledge-path configuration))
           (registry     (make-default-tool-registry))
           (conversation (conversation-create configuration :identifier "contract-reset"))
           (context      (make-instance 'tool-context :configuration configuration :worker nil
                                        :conversation conversation :registry registry)))
      (knowledge-define-contract configuration :id "global-probe" :tool "resource.read"
                                 :scope ':global)
      ;; A mixed transaction must retain the unrelated method alongside damaged metadata.
      (sexp-store:log-append
       pathname
       (list :transaction :version 1
             :records
             (list '(:contract-defined :version 1 :contract nil)
                   '(:diagnosis-defined :version 1 :diagnosis nil)
                   '(:diagnosis-recorded :version 1 :case nil)
                   (list :method-defined :version 1 :scope ':workspace
                         :workspace (knowledge--workspace configuration)
                         :time      (get-universal-time)
                         :method    (knowledge-make-method
                                     :id "retained-method" :task #("retain")
                                     :steps           (vector (json-object "step" "Retain evidence."))
                                     :schema-function (constantly nil)))))
       :repair-tail-p nil)
      (test-assert (handler-case (progn (knowledge-base-read configuration) nil)
                     (knowledge-error () t))
                   "malformed contract state actually prevents ordinary knowledge replay")
      (let* ((original (contract-recovery-tests--octets pathname))
             (refused  (memory-resource-tests--call
                        registry context :namespace "resource" :name "read"
                        :arguments '("uri" "memory:facts")))
             (reset    (memory-resource-tests--call
                        registry context :namespace "contracts" :name "reset"
                        :arguments nil)))
        (test-assert (not (tool-result-success-p refused))
                     "an ordinary tool is refused before emergency recovery")
        (test-assert (tool-result-success-p reset)
                     "the registered recovery tool bypasses broken contract matching")
        (let ((base     (knowledge-base-read configuration))
              (archives (remove-if-not
                         (lambda (candidate)
                           (uiop:string-prefix-p
                            (format nil "~A.before-contract-reset-" (pathname-name pathname))
                            (pathname-name candidate)))
                         (uiop:directory-files (uiop:pathname-directory-pathname pathname)))))
          (test-assert (= 1 (length archives)) "recovery preserves one original archive")
          (test-assert (equalp original (contract-recovery-tests--octets (first archives)))
                       "the archive preserves the entire original damaged log")
          (test-assert (platform-file-status-private-p
                        (platform-path-status *platform* (first archives) :follow-links-p t))
                       "the recovery archive is private")
          (test-assert (zerop (hash-table-count (knowledge-base-contracts-table base)))
                       "all scopes of contract state are abandoned")
          (test-assert (and (zerop (hash-table-count (knowledge-base-diagnoses-table base)))
                            (zerop (hash-table-count (knowledge-base-diagnosis-cases-table base))))
                       "saved probes and diagnosis history are abandoned")
          (test-assert (= 1 (hash-table-count (knowledge-base-claims base)))
                       "unrelated facts survive recovery")
          (test-assert (= 1 (hash-table-count (knowledge-base-methods-table base)))
                       "unrelated records inside a mixed transaction survive recovery"))
        (test-assert
         (tool-result-success-p
          (memory-resource-tests--call registry context :namespace "resource" :name "read"
                                       :arguments '("uri" "memory:facts")))
         "ordinary tool dispatch succeeds after recovery")
        (knowledge-define-contract configuration :id "new-probe" :tool "resource.read")
        (remhash (namestring pathname) *knowledge-base-cache*)
        (test-assert (= 1 (hash-table-count
                           (knowledge-base-contracts-table (knowledge-base-read configuration))))
                     "new contracts are usable after a fresh replay"))))
  nil)

(-> test-contract-reset-publication-failure () null)
(defun test-contract-reset-publication-failure ()
  "Failed recovery publication leaves the operational log intact and the archive available."
  (with-test-configuration (configuration)
    (knowledge-define-contract configuration :id "probe" :tool "resource.read")
    (let* ((pathname (configuration-knowledge-path configuration))
           (original (uiop:read-file-string pathname))
           (writer   (symbol-function 'sexp-store:log-write-streaming)))
      (test-assert
       (handler-case
           (test-call-with-function-replacements
            (list
             (list 'sexp-store:log-write-streaming
                   (lambda (target producer &rest options)
                     (apply writer target
                            (lambda (emit)
                              (funcall producer emit)
                              (error 'knowledge-error :code ':storage :subject "reset-publication"
                                     :message "Injected failure before atomic publication."))
                            options))))
            (lambda () (knowledge-reset-contracts configuration)))
         (knowledge-error () t))
       "a failure before publication reaches the caller")
      (test-assert (string= original (uiop:read-file-string pathname))
                   "failed recovery leaves the original log intact")
      (test-assert (= 1 (hash-table-count
                         (knowledge-base-contracts-table (knowledge-base-read configuration))))
                   "failed recovery leaves its existing contracts usable")))
  nil)

(-> test-contract-reset-command () null)
(defun test-contract-reset-command ()
  "Invoke registered recovery through the canonical command with malformed knowledge."
  (with-test-configuration (configuration)
    (multiple-value-bind (removed archive) (knowledge-reset-contracts configuration)
      (test-assert (and (zerop removed) (null archive))
                   "recovery handles a store without a knowledge log"))
    (let* ((application (make-instance 'application :configuration configuration
                                      :tool-registry               (make-default-tool-registry)))
           (pathname    (configuration-knowledge-path configuration))
           (command     (application-command-find "/reset-contracts"))
           (presented-p nil))
      (test-assert command "the canonical recovery command is registered")
      (test-assert (application-command-completion-entry command)
                   "the registered recovery command participates in completion")
      (sexp-store:log-append pathname '(:contract-defined :version 1 :contract nil)
                            :repair-tail-p nil)
      (test-call-with-function-replacements
       (list (list 'application-present
                   (lambda (target content &rest options)
                     (declare (ignore content options))
                     (test-assert (eq application target)
                                  "the recovery report reaches the requesting application")
                     (setf presented-p t))))
       (lambda () (application-operation-call application "reset-contracts")))
      (test-assert presented-p "canonical command invocation completes without semantic replay")
      (test-assert (zerop (hash-table-count
                           (knowledge-base-contracts-table (knowledge-base-read configuration))))
                   "command invocation publishes a readable store")))
  nil)

(-> test-contract-reset-reader-failure () null)
(defun test-contract-reset-reader-failure ()
  "Keep the operational log intact when unrelated persisted data cannot be read safely."
  (with-test-configuration (configuration)
    (knowledge-define-contract configuration :id "probe" :tool "resource.read")
    (let ((pathname (configuration-knowledge-path configuration)))
      (with-open-file (stream pathname :direction ':output :if-exists ':append)
        (write-line "#.(error \"Read-time evaluation must not run.\")" stream))
      (let ((original (contract-recovery-tests--octets pathname)))
        (test-assert (handler-case (progn (knowledge-reset-contracts configuration) nil)
                       (knowledge-error () t))
                     "unsafe reader syntax is rejected with evaluation disabled")
        (test-assert (equalp original (contract-recovery-tests--octets pathname))
                     "an unreadable record cannot cause a partial replacement"))))
  nil)
