(in-package #:autolith)

;;;; -- Operating System Permission Failures --

(-> test-telemetry-os-permission-failure-hooks () null)
(defun test-telemetry-os-permission-failure-hooks ()
  "Real OS-denied reads, path resolution, edit staging and parent creation retain permission codes."
  (test-telemetry--call
   (lambda (configuration)
     (with-test-fixture (':permission-denials "telemetry OS permission refusals")
       (let* ((workspace (merge-pathnames "workspace/" (test-configuration-root configuration)))
              (file      (merge-pathnames "unreadable.txt" workspace))
              (directory (merge-pathnames "unreadable/" workspace))
              (nested    (merge-pathnames "unreadable/child.txt" workspace))
              (read-only (merge-pathnames "read-only/" workspace))
              (editable  (merge-pathnames "file.txt" read-only))
              (registry  (make-default-tool-registry))
              (context   (make-instance 'tool-context :configuration configuration :registry registry
                                        :conversation (conversation-create configuration) :worker nil)))
         (ensure-directories-exist directory)
         (ensure-directories-exist read-only)
         (setf (config :working-directory configuration) workspace)
         (test-write-text file "private content")
         (test-write-text nested "private nested content")
         (test-write-text editable "original")
         (let* ((read-result      (test-resource-call
                                   registry context
                                   :namespace "resource"
                                   :name      "read"
                                   :arguments (list "uri" "workspace:read-only/file.txt")))
                (revision         (test-resource-field (tool-result-content read-result) "Revision: "))
                (missing-result   (test-resource-call
                                   registry context
                                   :namespace "resource"
                                   :name      "read"
                                   :arguments (list "uri" "workspace:read-only/new/file.txt")))
                (missing-revision (test-resource-field (tool-result-content missing-result) "Revision: ")))
           (test-assert (tool-result-success-p read-result) "The edit target is readable before staging.")
           (test-assert (eq (tool-result-error-code missing-result) ':not-found) "The absent parent is observed before editing.")
           (unwind-protect
                (progn
                  (test-fixture-set-file-mode *platform* file 0)
                  (test-fixture-set-file-mode *platform* directory 0)
                  (test-fixture-set-file-mode *platform* read-only #o555)
                  (multiple-value-bind (spans outputs body)
                      (test-telemetry-hooks--tool-attempts
                       configuration
                       (append
                        (loop for uri in '("workspace:unreadable.txt" "workspace:unreadable/" "workspace:unreadable/child.txt")
                              for index from 1
                              collect (agent-test-call :call-id   (format nil "os-~D" index)
                                                       :namespace "resource" :name "read"
                                                       :arguments (json-encode (json-object "uri" uri))))
                        (list (agent-test-call
                               :call-id "os-edit" :namespace "resource" :name "edit"
                               :arguments (json-encode
                                           (json-object "uri" "workspace:read-only/file.txt" "base-revision" revision
                                                        "operations" (vector (json-object "op" "replace-lines" "start-line" 1
                                                                                          "end-line" 1 "content" "changed")))))
                              (agent-test-call
                               :call-id "os-create-parent" :namespace "resource" :name "edit"
                               :arguments (json-encode
                                           (json-object "uri" "workspace:read-only/new/file.txt" "base-revision" missing-revision
                                                        "operations" (vector (json-object "op" "replace-empty" "content" "created")))))))
                       :conversation (tool-context-conversation context) :registry registry)
                    (test-assert (= (length spans) 5) "Read, resolution, staging and parent-directory OS refusals are exported.")
                    (dolist (span spans)
                      (test-assert
                       (and (equal (test-telemetry--attribute span "autolith.tool.outcome") "error")
                            (equal (test-telemetry--attribute span "autolith.tool.error_code") "os-permission-denied")
                            (equal (test-telemetry--attribute span "error.type") "permission"))
                       "EACCES is an OS failure, not a policy denial."))
                    (test-assert (and (= (length outputs) 5)
                                      (every (lambda (output) (search "failed:" output)) outputs))
                                 "OS refusals retain ordinary model-visible failure replies.")
                    (test-assert (not (search "private content" body)) "File contents stay off the wire.")))
             (test-fixture-set-file-mode *platform* file #o600)
             (test-fixture-set-file-mode *platform* directory #o700)
             (test-fixture-set-file-mode *platform* read-only #o700)))
         (test-assert (equal "original" (workspace-file--read-content editable))
                      "A refused staging write preserves the readable original file.")
         (test-assert (not (probe-file (merge-pathnames "new/" read-only)))
                      "Refused parent creation leaves no directory or staged file.")))))
  nil)
