(in-package #:autolith-supervisor)

;;;; -- Parent Plan Boundary Checks --

(serapeum/types:-> knowledge-plan-tests--request (hash-table &key (:scope keyword)) parent-proto:knowledge-request)
(defun knowledge-plan-tests--request (fields &key (scope ':workspace))
  "Build an actual bounded READ request for plan data."
  (parent-proto:make-knowledge-request :operation ':read :scope scope :view "plan"
                                      :fields (protocol-data->map fields)))

(serapeum/types:-> knowledge-plan-tests--reply (knowledge-service hash-table) hash-table)
(defun knowledge-plan-tests--reply (service fields)
  "Pass plan data through production authorization and request dispatch."
  (let ((reply (knowledge-service-request service (knowledge-plan-tests--request fields))))
    (protocol-value->data (common-proto:make-value :map-value (parent-proto:knowledge-response.fields reply)))))

(serapeum/types:-> run-knowledge-plan-tests () integer)
(defun run-knowledge-plan-tests ()
  "Check typed plan semantics, durable receipts, scope limits and graph drift."
  (let* ((root      (merge-pathnames (format nil "parent-plan-~A/" (memory--make-identifier))
                                     (uiop:temporary-directory)))
         (workspace (merge-pathnames "workspace/" root))
         (context   (make-memory-store-context :memory-path    (merge-pathnames "memory.sexp" root)
                                               :knowledge-path (merge-pathnames "knowledge.sexp" root)
                                               :workspace workspace :mode "active"))
         (service   (make-knowledge-service :context context :session-id "plan-test"))
         (checks    0))
    (ensure-directories-exist workspace)
    (unwind-protect
         (labels ((check (value)
                    (incf checks)
                    (assert value () "Parent plan check ~D failed." checks))

                  (data (action steps &key canonical previous active)
                    (let ((fields (argo:json-object "action" action "steps" steps
                                                    "canonical" canonical "active" active)))
                      (when previous (setf (gethash "previous" fields) previous))
                      fields))

                  (call-plan (action steps &key canonical previous active)
                    (knowledge-plan-tests--reply service
                                                (data action steps :canonical canonical :previous previous :active active)))

                  (denied (fields &optional (scope ':workspace))
                    (handler-case
                        (progn (knowledge-service-request service
                                                         (knowledge-plan-tests--request fields :scope scope)) nil)
                      (knowledge-service-error (condition)
                        (knowledge-service-error-code condition)))))
           (knowledge-define-schema context
                                    (knowledge-make-schema :predicate "build.value"
                                                           :arguments '((:name "value" :type :number))))
           (knowledge-define-schema context
                                    (knowledge-make-schema :predicate "build.verified"
                                                           :arguments '((:name "project" :type :entity))))
           (let* ((raw     (vector (argo:json-object "text" "Use {?value}" "status" "doing" "id" "build"
                                                     "requires" #(#("build.value" "?value"))
                                                    "verify" #("build.verified" "atlas"))))
                  (blocked (call-plan "prepare" raw)))
             (check (search "blocked" (gethash "rendered" blocked)))
             (multiple-value-bind (claim support) (knowledge-assert context '("build.value" 1/3) :basis ':assumption)
               (declare (ignore claim))
               (let* ((ready (call-plan "prepare" raw))
                      (steps (gethash "steps" ready)))
                 (check (search (format nil "Use ~A" (knowledge-term-display 1/3)) (gethash "rendered" ready)))
                 (check (null (gethash "receipt" (aref steps 0))))
                 (check (= 1/3 (second (first (knowledge-plan-data->term
                                               (knowledge-plan-term->data '(("build.value" 1/3)))))))))
               (setf (gethash "status" (aref raw 0)) "done")
               (let* ((done   (call-plan "prepare" raw))
                      (stored (gethash "steps" done))
                      (token  (gethash "receipt" (aref stored 0))))
                 (check (non-empty-string-p token))
                 (check (search "not observed" (gethash "rendered" done)))
                 (knowledge-assert context '("build.verified" "atlas") :basis ':assumption)
                 (check (not (search "not observed" (gethash "rendered" (call-plan "render" stored :canonical t)))))
                 (check (equal token (gethash "receipt"
                                              (aref (gethash "steps" (call-plan "prepare" raw :previous stored)) 0))))
                 (knowledge-retract context support :reason "New build value")
                 (knowledge-assert context '("build.value" 2/3) :basis ':assumption)
                 (let ((drift (call-plan "render" stored :canonical t)))
                   (check (search (format nil "Use ~A" (knowledge-term-display 1/3)) (gethash "rendered" drift)))
                   (check (search "no longer holds" (gethash "rendered" drift))))
                 (check (= 1 (length (gethash "changes" (call-plan "changes" stored :canonical t :active t)))))
                 (check (zerop (length (gethash "changes" (call-plan "changes" stored :canonical t :active t)))))
                 (setf service (make-knowledge-service :context
                                                       (make-memory-store-context
                                                        :memory-path    (merge-pathnames "memory.sexp" root)
                                                        :knowledge-path (merge-pathnames "knowledge.sexp" root)
                                                        :workspace workspace :mode "active")
                                                       :session-id "plan-test"))
                 (check (search "no longer holds" (gethash "rendered" (call-plan "render" stored :canonical t))))
                 (let ((copy (map 'vector (lambda (item) (alexandria:copy-hash-table item)) stored)))
                   (setf (gethash "text" (aref copy 0)) "Forged step")
                   (check (eq ':permission-denied (denied (data "render" copy :canonical t)))))
                 (let ((other (make-knowledge-service :context context :session-id "other-plan")))
                   (check (search "no longer holds"
                                  (gethash "rendered" (knowledge-plan-tests--reply
                                                       other (data "render" stored :canonical t))))))
                 (dolist (other (list (make-knowledge-service :context context :session-id "plan-test"
                                                             :allow-global-p t)
                                     (make-knowledge-service
                                      :context    (make-memory-store-context
                                                   :memory-path    (merge-pathnames "memory.sexp" root)
                                                   :knowledge-path (merge-pathnames "knowledge.sexp" root)
                                                   :workspace (merge-pathnames "other/" root) :mode "active")
                                      :session-id "plan-test")))
                   (check (handler-case
                              (progn (knowledge-plan-tests--reply other (data "render" stored :canonical t)) nil)
                            (knowledge-service-error (condition)
                              (eq ':permission-denied (knowledge-service-error-code condition))))))
                 (let ((current stored))
                   (dotimes (index 130)
                     (let ((changing (argo:json-object "text" (format nil "Changing ~D" index)
                                                      "status" "done" "requires" #(#("build.value" "?value")))))
                       (setf current (gethash "steps"
                                              (call-plan "prepare" (vector (aref current 0) changing))))))
                   (check (equal token (gethash "receipt" (aref current 0))))
                   (check (search "no longer holds"
                                  (gethash "rendered" (call-plan "render" stored :canonical t)))))
                 (setf (gethash "text" (aref raw 0)) "Changed {?value}")
                 (check (not (equal token (gethash "receipt"
                                                    (aref (gethash "steps" (call-plan "prepare" raw :previous stored)) 0))))))))
           (dolist (mode '("active" "passive" "off"))
             (setf (slot-value (knowledge-service-context service) 'mode) mode)
             (let ((plain (vector (argo:json-object "text" "Plain" "status" "pending"))))
               (check (search "Plain" (gethash "rendered" (call-plan "prepare" plain)))))
             (unless (equal mode "active")
               (check (zerop (length (gethash "changes" (call-plan "changes" #() :canonical t :active t)))))))
           (check (equal "No active plan." (gethash "rendered" (call-plan "prepare" #()))))

           (let ((stored (vector (argo:json-object "text" "Old schema" "status" "pending"
                                                   "requires" #(#("removed.predicate" "atlas"))))))
             (check (search "blocked" (gethash "rendered" (call-plan "render" stored :canonical t))))
             (check (equal "No active plan."
                           (gethash "rendered" (call-plan "prepare" #() :previous stored)))))
           (check (eq ':invalid-request (denied (data "eval" #()))))

           (check (eq ':invalid-request (denied (data "prepare" #() :canonical t))))
           (check (eq ':invalid-request (denied (data "render" #() :canonical nil))))
           (check (eq ':permission-denied (denied (data "render" #()) ':global)))
           (check (eq ':resource-limit (denied (data "prepare" (make-array 33 :initial-element nil)))))
           (check (eq ':invalid-request
                      (denied (data "prepare" (vector (argo:json-object "text" "Unknown" "status" "pending"
                                                                        "requires" #(#("missing"))))))))
           (check (eq ':invalid-request
                      (denied (data "prepare" (vector (argo:json-object "text" "Bad number" "status" "pending"
                                                                        "requires" #(#("build.value" "bad"))))))))
           (check (eq ':invalid-request
                      (denied (data "prepare" (vector (argo:json-object "text" "Large" "status" "pending"
                                                                        "requires" (make-array 33 :initial-element #("build.value" 1))))))))
           (let* ((schema (knowledge-make-schema :predicate "private.flag"
                                                 :arguments '((:name "project" :type :entity))))
                  (global (knowledge-define-schema context schema :scope ':global)))
             (declare (ignore global))
             (check (eq ':invalid-request
                        (denied (data "prepare" (vector (argo:json-object "text" "Private" "status" "pending"
                                                                          "requires" #(#("private.flag" "atlas"))))))))))
      (dolist (directory (list workspace root))
        (dolist (file (uiop:directory-files directory)) (delete-file file))
        (when (probe-file directory) (uiop:delete-empty-directory directory))))
    (format t "Parent plan: ~D checks passed.~%" checks)
    checks))
