(in-package #:autolith)

;;;; -- Refinement Fixtures --

(defclass refinement-test-state ()
  ((changes :initform 0 :accessor refinement-test-changes
            :documentation "Actual adaptation version.")
   (behavior :initform ':success :accessor refinement-test-behavior
             :documentation "Owner success, failure, pending or interruption mode.")
   (context :initform nil :accessor refinement-test-context
            :documentation "Context observed by the delegated owner.")
   (decision :initform ':full-access :accessor refinement-test-decision
             :documentation "Current user authorization decision."))
  (:documentation "Independently owned registry fixture state."))

(defclass refinement-test-tool (tool)
  ((state :initarg :state :reader refinement-test-tool-state
          :documentation "Shared adaptation state."))
  (:documentation "An existing owner represented at the registry boundary."))

(defclass refinement-test-pending-result (tool-result) ()
  (:documentation "A handoff rather than completed owner work."))

(defmethod tool-result-details ((result refinement-test-pending-result))
  "Identify a pending owner operation."
  (declare (ignore result))
  (list :kind ':execution-job :id "fixture-job"))

(defmethod tool-execute ((tool refinement-test-tool) (context tool-context) (arguments hash-table))
  "Perform an effect or return actual fixture task measurements."
  (declare (ignore arguments))
  (let ((state (refinement-test-tool-state tool)))
    (setf (refinement-test-context state) context)
    (cond
      ((equal (tool-name tool) "diff")
       (tool-success (format nil "Adaptation version ~D" (refinement-test-changes state))))
      ((eq (refinement-test-behavior state) :fail)
       (tool-failure "Task benchmark exceeded its operation budget."))
      ((eq (refinement-test-behavior state) :pending)
       (make-instance 'refinement-test-pending-result :success-p t :content "Owner job started."))
      ((equal (tool-name tool) "exercise")
       (tool-success "Task benchmark used 8 operations; unchanged baseline used 14."))
      (t
       (incf (refinement-test-changes state))
       (when (eq (refinement-test-behavior state) :interrupt)
         (throw 'refinement-test-interruption nil))
       (tool-success "Owner mechanical checks succeeded.")))))

(-> refinement-tests--operation (string string) string)
(defun refinement-tests--operation (namespace name)
  "Build a portable existing-owner call."
  (json-encode (json-object "namespace" namespace "name" name "arguments" "{}")))

(-> refinement-tests--fixture (function) null)
(defun refinement-tests--fixture (function)
  "Run FUNCTION with independent configuration, context and owner state."
  (with-test-configuration (configuration root)
    (let* ((configuration (configuration-copy configuration :working-directory root))
           (registry (make-instance 'tool-registry))
           (state (make-instance 'refinement-test-state))
           (context (make-instance 'tool-context :configuration configuration :worker nil :registry registry
                                  :conversation (conversation-create configuration)
                                  :command-authorization-function
                                  (lambda (command directory)
                                    (declare (ignore command directory))
                                    (refinement-test-decision state)))))
      (dolist (owner '(("self" . "define") ("self" . "commit") ("self" . "diff")
                       ("self" . "exercise") ("resource" . "edit")))
        (tool-registry-register registry
                                (make-instance 'refinement-test-tool :namespace (first owner) :name (rest owner)
                                               :state state :description "Fixture owner"
                                               :parameters (tool-object-schema (json-object) nil))))
      (refinement-register-tools registry)
      (funcall function configuration context state)))
  nil)

(-> refinement-tests--proposal (configuration tool-context &key (:target keyword)) list)
(defun refinement-tests--proposal (configuration context &key (target :live-change))
  "Propose an adaptation with concrete trajectory evidence and task criterion."
  (refinement-propose configuration :owner (conversation-identifier (tool-context-conversation context))
                      :target target :evidence '("conversation:current sequences 12-24" "inference:fixture")
                      :reason "Simpler contextual guidance was tried first; repeated extra tool rounds persist."
                      :benefit "Complete equivalent work in fewer tool operations."
                      :criterion "Complete the fixture task in at most 10 operations, baseline 14."))

(-> refinement-tests--run (tool-context list keyword &key (:scope (option keyword))) list)
(defun refinement-tests--run (context proposal phase &key scope)
  "Invoke the existing owner for this lifecycle phase."
  (refinement-run context :identifier (getf proposal :id) :revision (getf proposal :revision)
                  :phase phase :scope scope
                  :guard (and (eq phase :experiment) (refinement-tests--operation "self" "diff"))
                  :operation (if (and (eq phase :promote) (eq scope :project))
                                 (refinement-tests--operation "resource" "edit")
                                 (refinement-tests--operation "self"
                                                              (case phase (:experiment "define")
                                                                    (:evaluate "exercise") (:promote "commit"))))))

(-> refinement-tests--assess (tool-context list keyword) list)
(defun refinement-tests--assess (context proposal verdict)
  "Supply a trusted reviewer of the actual fixture measurements."
  (refinement-assess context :identifier (getf proposal :id) :revision (getf proposal :revision)
                     :reviewer (lambda (record)
                                 (fiveam:is (equal (getf record :criterion) (getf proposal :criterion)))
                                 (fiveam:is (not (null (search "8 operations" (getf record :observation)))))
                                 (values verdict
                                         (if (eq verdict :passed)
                                             "Observed 8 <= 10 operations with the task completed."
                                             "The task produced an incorrect artifact despite fewer operations.")))))

(-> refinement-tests--error-code (function) (option keyword))
(defun refinement-tests--error-code (function)
  "Return a typed rejection code or NIL on unexpected success."
  (handler-case (progn (funcall function) nil)
    (refinement-error (condition) (refinement-error-code condition))))

;;;; -- Lifecycle Contracts --

(-> test-refinement-progression nil null)
(defun test-refinement-progression ()
  "Require separate observations and task assessment for each promotion scope."
  (refinement-tests--fixture
   (lambda (configuration context state)
     (let* ((proposal (refinement-tests--proposal configuration context))
            (experiment (refinement-tests--run context proposal :experiment))
            (observed (refinement-tests--run context experiment :evaluate)))
       (fiveam:is (eq (getf observed :state) :observed))
       (fiveam:is (null (getf observed :task-evaluation)))
       (fiveam:is (eq (refinement-test-context state) context))
       (fiveam:is (eq (getf (first (getf observed :mechanical-checks)) :claim) :owner-execution-only))
       (fiveam:is (eq :invalid (refinement-tests--error-code
                               (lambda () (refinement-tests--run context observed :promote :scope :project)))))
       (let* ((passed (refinement-tests--assess context observed :passed))
              (project (refinement-tests--run context passed :promote :scope :project)))
         (fiveam:is (eq (getf project :scope) :project))
         (fiveam:is (null (getf project :task-evaluation)))
         (fiveam:is (eq :invalid (refinement-tests--error-code
                                 (lambda () (refinement-tests--run context project :promote :scope :global)))))
         (let* ((observed (refinement-tests--run context project :evaluate))
                (passed (refinement-tests--assess context observed :passed))
                (global (refinement-tests--run context passed :promote :scope :global)))
           (fiveam:is (eq (getf global :scope) :global))
           (fiveam:is (= 3 (refinement-test-changes state)))
           (fiveam:is (equal global (refinement-find configuration (getf proposal :id))))))))))

(-> test-refinement-no-op nil null)
(defun test-refinement-no-op ()
  "No useful lesson is an ordinary durable result with no adaptation effect."
  (refinement-tests--fixture
   (lambda (configuration context state)
     (let ((proposal (refinement-propose
                      configuration :owner (conversation-identifier (tool-context-conversation context))
                      :target :none :evidence '("Task completed without recurring friction.")
                      :reason "No reliable useful adaptation was identified.")))
       (fiveam:is (eq (getf proposal :state) :no-op))
       (fiveam:is (equal (list proposal) (refinement-list configuration)))
       (fiveam:is (eq :invalid (refinement-tests--error-code
                               (lambda () (refinement-tests--run context proposal :experiment)))))
       (fiveam:is (zerop (refinement-test-changes state)))))))

(-> test-refinement-failed-evaluation nil null)
(defun test-refinement-failed-evaluation ()
  "Neither failed measurement nor rejected task artifacts can promote."
  (refinement-tests--fixture
   (lambda (configuration context state)
     (let* ((proposal (refinement-tests--proposal configuration context))
            (experiment (refinement-tests--run context proposal :experiment)))
       (setf (refinement-test-behavior state) :fail)
       (let ((failed (refinement-tests--run context experiment :evaluate)))
         (fiveam:is (eq (getf failed :state) :failed))
         (fiveam:is (null (getf failed :task-evaluation)))
         (fiveam:is (eq :invalid (refinement-tests--error-code
                                 (lambda () (refinement-tests--assess context failed :passed)))))
         (setf (refinement-test-behavior state) :success)
         (let* ((observed (refinement-tests--run context failed :evaluate))
                (failed (refinement-tests--assess context observed :failed)))
           (fiveam:is (eq (getf failed :state) :failed))
           (fiveam:is (eq :invalid (refinement-tests--error-code
                                   (lambda () (refinement-tests--run context failed :promote :scope :project)))))
           (fiveam:is (= 1 (refinement-test-changes state)))))))))

(-> test-refinement-stale-promotion nil null)
(defun test-refinement-stale-promotion ()
  "Reject stale revisions, skipped scopes and externally changed adaptations."
  (refinement-tests--fixture
   (lambda (configuration context state)
     (let* ((proposal (refinement-tests--proposal configuration context))
            (experiment (refinement-tests--run context proposal :experiment))
            (observed (refinement-tests--run context experiment :evaluate))
            (passed (refinement-tests--assess context observed :passed)))
       (fiveam:is (eq :stale (refinement-tests--error-code
                             (lambda () (refinement-tests--run context observed :promote :scope :project)))))
       (fiveam:is (eq :invalid (refinement-tests--error-code
                               (lambda () (refinement-tests--run context passed :promote :scope :global)))))
       (incf (refinement-test-changes state))
       (fiveam:is (eq :stale (refinement-tests--error-code
                             (lambda () (refinement-tests--run context passed :promote :scope :project)))))
       (fiveam:is (equal passed (refinement-find configuration (getf proposal :id))))))))

(-> test-refinement-authority nil null)
(defun test-refinement-authority ()
  "Preserve explicit user authorization and session ownership."
  (refinement-tests--fixture
   (lambda (configuration context state)
     (let ((proposal (refinement-tests--proposal configuration context)))
       (setf (refinement-test-decision state) :sandboxed)
       (fiveam:is (eq :authority (refinement-tests--error-code
                                 (lambda () (refinement-tests--run context proposal :experiment)))))
       (fiveam:is (zerop (refinement-test-changes state)))
       (setf (refinement-test-decision state) :full-access)
       (let* ((experiment (refinement-tests--run context proposal :experiment))
              (observed (refinement-tests--run context experiment :evaluate))
              (passed (refinement-tests--assess context observed :passed)))
         (setf (refinement-test-decision state) :deny)
         (fiveam:is (eq :authority (refinement-tests--error-code
                                   (lambda () (refinement-tests--run context passed :promote :scope :project)))))
         (let ((foreign (make-instance 'tool-context :configuration configuration :worker nil
                                       :registry (tool-context-registry context)
                                       :conversation (conversation-create configuration))))
           (fiveam:is (eq :authority (refinement-tests--error-code
                                     (lambda () (refinement-tests--run foreign passed :evaluate)))))))))))

(-> test-refinement-interrupted-recovery nil null)
(defun test-refinement-interrupted-recovery ()
  "Persist intent before effects and recover without blindly replaying an effect."
  (refinement-tests--fixture
   (lambda (configuration context state)
     (let ((proposal (refinement-tests--proposal configuration context)))
       (setf (refinement-test-behavior state) :interrupt)
       (catch 'refinement-test-interruption (refinement-tests--run context proposal :experiment))
       (let* ((interrupted (refinement-find configuration (getf proposal :id)))
              (recovered (refinement-recover context :identifier (getf proposal :id)
                                             :revision (getf interrupted :revision)
                                             :evidence "Inspected the original owner and discarded its partial adaptation.")))
         (fiveam:is (eq (getf interrupted :state) :applying))
         (fiveam:is (eq (getf recovered :state) :failed))
         (fiveam:is (null (getf recovered :task-evaluation)))
         (fiveam:is (= 1 (refinement-test-changes state)))
         (fiveam:is (eq :invalid (refinement-tests--error-code
                                 (lambda () (refinement-tests--run context recovered :evaluate)))))
         (setf (refinement-test-behavior state) :success)
         (fiveam:is (eq :experiment (getf (refinement-tests--run context recovered :experiment) :state))))))))

(-> test-refinement-pending-owner nil null)
(defun test-refinement-pending-owner ()
  "A pending owner job does not complete an adaptation."
  (refinement-tests--fixture
   (lambda (configuration context state)
     (let ((proposal (refinement-tests--proposal configuration context)))
       (setf (refinement-test-behavior state) :pending)
       (let ((pending (refinement-tests--run context proposal :experiment)))
         (fiveam:is (eq (getf pending :state) :blocked))
         (fiveam:is (null (getf pending :binding)))
         (fiveam:is (eq :invalid (refinement-tests--error-code
                                 (lambda () (refinement-tests--run context pending :evaluate))))))))))

(-> test-refinement-tool-boundary nil null)
(defun test-refinement-tool-boundary ()
  "Invoke delegation through the unified operation while rejecting model efficacy claims."
  (refinement-tests--fixture
   (lambda (configuration context state)
     (let* ((registry (tool-context-registry context))
            (proposal (refinement-tests--proposal configuration context :target :memory))
            (claim (tool-registry-execute-call
                    registry (json-object "namespace" "self" "name" "refine"
                                          "arguments" "{\"action\":\"assess\",\"verdict\":\"passed\"}") context)))
       (fiveam:is (not (tool-result-success-p claim)))
       (fiveam:is (eq :invalid (refinement-tests--error-code
                               (lambda () (refinement-tests--run context proposal :experiment)))))
       (let ((result (tool-registry-execute-call
                      registry (json-object "namespace" "self" "name" "refine"
                                            "arguments" (json-encode
                                                         (json-object "action" "experiment" "id" (getf proposal :id)
                                                                      "revision" 1
                                                                      "operation" (refinement-tests--operation "resource" "edit")
                                                                      "guard" (refinement-tests--operation "self" "diff")))) context)))
         (fiveam:is (tool-result-success-p result))
         (fiveam:is (= 1 (refinement-test-changes state)))
         (fiveam:is (eq (refinement-test-context state) context)))))))

(-> test-refinement-journal-integrity nil null)
(defun test-refinement-journal-integrity ()
  "Handle interrupted tails, confine workspaces and reject discontinuous revisions."
  (refinement-tests--fixture
   (lambda (configuration context state)
     (declare (ignore state))
     (let ((proposal (refinement-tests--proposal configuration context)))
       (with-open-file (stream (configuration-journal-path configuration)
                               :direction :output :if-exists :append :external-format :utf-8)
         (write-string "(:refinement :id \"incomplete" stream))
       (fiveam:is (equal (list proposal) (refinement-list configuration)))
       (let ((other (configuration-copy configuration
                                        :working-directory (merge-pathnames "other/" (config ':working-directory configuration)))))
         (fiveam:is (null (refinement-list other))))
       (let ((broken (copy-tree proposal)))
         (setf (getf broken :revision) 3)
         (mutation-journal-append configuration (cons :refinement broken))
         (fiveam:is (eq :corrupt (refinement-tests--error-code
                                (lambda () (refinement-list configuration))))))))))

(-> test-refinement-immutable-registry nil null)
(defun test-refinement-immutable-registry ()
  "Offer refinement in the mutable default registry and omit it from immutable mode."
  (with-test-configuration (configuration root)
    (let* ((configuration (configuration-copy configuration :working-directory root))
           (mutable (make-default-tool-registry :configuration configuration))
           (immutable (make-default-tool-registry :configuration configuration :immutable-p t)))
      (unwind-protect
           (let* ((conversation (conversation-create configuration))
                  (context (make-instance 'tool-context :configuration configuration :worker nil
                                          :registry mutable :conversation conversation)))
             (fiveam:is (not (null (tool-registry-find mutable "self" "refine"))))
             (fiveam:is (null (tool-registry-find immutable "self" "refine")))
             (fiveam:is (tool-result-success-p
                         (tool-registry-execute-call
                          mutable (json-object "namespace" "self" "name" "refine"
                                               "arguments" "{\"action\":\"status\"}") context))))
        (tool-registry-close-runtime-state mutable)
        (tool-registry-close-runtime-state immutable))))
  nil)
