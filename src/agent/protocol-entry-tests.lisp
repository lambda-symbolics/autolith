;;;; -- Fresh Child Launch Fixture --

(in-package #:autolith)

(defclass agent-entry-test-effect-tool (tool)
  ()
  (:documentation "One test-owned file effect inside a real provider/tool turn."))

(defmethod agent-observer-authorize-tool
    ((observer agent-protocol-observer) (tool agent-entry-test-effect-tool) (arguments hash-table))
  "Permit only the fixed fixture tool; the parent never evaluates its Lisp."
  (declare (ignore observer tool arguments))
  ':allow)

(-> agent-entry-test--worker-form (string) string)
(defun agent-entry-test--worker-form (path)
  "Run the real worker and record its identity and optional native policy probes."
  (let ((private (uiop:getenv "AUTOLITH_LAUNCH_TEST_PRIVATE"))
        (outside (uiop:getenv "AUTOLITH_LAUNCH_TEST_OUTSIDE")))
    (with-standard-io-syntax
      (let ((*package* (find-package '#:autolith)))
       (write-to-string
        `(let ((report (list (autolith::current-process-id))))
           ,@(when private
               `((labels ((readable-p (path)
                            (handler-case
                                (with-open-file (stream path) (read-char stream nil nil) t)
                              (file-error () nil))))
                   (setf report
                         (append report
                                 (list (readable-p ,private) (readable-p ,outside)
                                       (sb-alien:alien-funcall
                                        (sb-alien:extern-alien "setsid" (function sb-alien:int)))
                                       (sb-alien:alien-funcall
                                        (sb-alien:extern-alien "setpgid"
                                                              (function sb-alien:int sb-alien:int sb-alien:int))
                                        0 0)))))))
           (with-open-file (stream ,(concatenate 'string path ".worker")
                                   :direction ':output :if-exists ':supersede)
             (write report :stream stream))))))))

(-> agent-entry-test--records (tool-context) null)
(defun agent-entry-test--records (context)
  "Use the normal resource tools and their observation guards over the parent channel."
  (let* ((registry    (default-tools--register-workspace (make-instance 'tool-registry)))
         (read-result (tool-execute (tool-registry-find registry "resource" "read")
                                    context (json-object "uri" "agenda:current")))
         (content     (tool-result-content read-result))
         (start       (search "Revision: " content)))
    (assert (tool-result-success-p read-result))
    (assert start)
    (incf start (length "Revision: "))
    (let* ((end      (or (position #\Newline content :start start) (length content)))
           (revision (subseq content start end))
           (result   (tool-execute
                      (tool-registry-find registry "resource" "edit") context
                      (json-object "uri" "agenda:current" "base-revision" revision
                                   "operations" (vector (json-object "op" "agenda-add"
                                                                     "text" "Real child resource edit"
                                                                     "status" "note"))))))
      (assert (tool-result-success-p result))))
  nil)

(defmethod tool-execute
    ((tool agent-entry-test-effect-tool) (context tool-context) (arguments hash-table))
  "Commit one visible file effect, then optionally crash before reporting its result."
  (declare (ignore tool arguments))
  (assert (null (uiop:getenv "AUTOLITH_LAUNCH_PARENT_SECRET")))
  (let ((path (uiop:getenv "AUTOLITH_LAUNCH_TEST_EFFECT")))
    (assert path)
    (when (uiop:getenv "AUTOLITH_LAUNCH_TEST_RECORDS")
      (agent-entry-test--records context))
    (let* ((worker (lisp-worker-manager-worker (tool-context-worker context) "launch-fixture"))
           (form   (agent-entry-test--worker-form path))
           (reply  (lisp-worker-request worker ':eval (list :forms (list form)))))
      (unless (eq (getf (rest reply) :status) ':ok)
        (format *error-output* "~&Fixture worker failed: ~S~%" reply)
        (finish-output *error-output*)
        (error 'agent-protocol-error :reason ':fixture-worker-failed)))
    (with-open-file (stream path :direction ':output :if-exists ':append :if-does-not-exist ':create)
      (write-line "effect" stream)
      (finish-output stream)))
  (when (equal (uiop:getenv "AUTOLITH_LAUNCH_TEST_MODE") "uncertain")
    (sb-ext:exit :code 23 :abort t))
  (tool-success "effect committed"))

(-> agent-entry-test--registry () tool-registry)
(defun agent-entry-test--registry ()
  "Build a child-local registry with one fixed effect and no parent operation."
  (let ((registry (make-instance 'tool-registry)))
    (tool-registry-register
     registry (make-instance 'agent-entry-test-effect-tool
                             :namespace "fixture" :name "effect" :description "Commit the fixture effect."
                             :parameters (json-object "type" "object" "properties" (json-object)
                                                      "additionalProperties" nil)))
    registry))

(-> agent-entry-test-main (list) null)
(defun agent-entry-test-main (arguments)
  "Boot a source-matched fresh child, then run the real default provider/tool loop."
  (let ((root (uiop:getenv "AUTOLITH_SOURCE_ROOT")))
    (unless (and root
                 (sbcl-generations:image-build-record-compatible-p
                  (sbcl-generations:image-running-build-record) (pathname root)
                  :inputs #'active-image-source-paths))
      (error 'agent-protocol-error :reason ':fixture-source-mismatch)))
  (when (equal (rest arguments) '("--worker"))
    (worker-main)
    (uiop:quit 0))
  (when arguments
    (error 'agent-protocol-error :reason ':invalid-fixture-arguments))
  (agent-protocol-entry-run :tool-registry (agent-entry-test--registry))
  (uiop:quit 0))

(-> agent-entry-test-install (pathname pathname) pathname)
(defun agent-entry-test-install (source-root core)
  "Build and probe a fixture image and its source manifest in this isolated builder."
  (image-install source-root core :inputs #'active-image-source-paths
                 :toplevel #'agent-entry-test-main :prepare #'active-image--prepare-saver
                 :probe-runner            (make-sbcl-core-probe-runner :command (active-image--sbcl-command))
                 :probe-argument          "--autolith-launch-fixture-probe"
                 :publish-function        #'active-image--publish-core
                 :write-function          #'active-image--write-manifest))
