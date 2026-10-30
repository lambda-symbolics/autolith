(in-package #:autolith-supervisor)

;;;; -- Parent Context and Method Checks --

(serapeum/types:-> knowledge-context-tests--request (keyword hash-table) parent-proto:knowledge-request)
(defun knowledge-context-tests--request (operation fields)
  "Build a data-only request for the selected workspace."
  (parent-proto:make-knowledge-request
   :operation operation :scope ':workspace
   :fields (common-proto:value.map-value (protocol-data->value fields))))

(serapeum/types:-> knowledge-context-tests--reply (knowledge-service keyword hash-table) hash-table)
(defun knowledge-context-tests--reply (service operation fields)
  "Send one request through production dispatch and decode its response."
  (let ((response (knowledge-service-request service (knowledge-context-tests--request operation fields))))
    (protocol-value->data (common-proto:make-value
                           :map-value (parent-proto:knowledge-response.fields response)))))

(serapeum/types:-> run-knowledge-context-tests () integer)
(defun run-knowledge-context-tests ()
  "Check active recall, parent scope, durable viewers and portable method expansion."
  (let* ((root      (merge-pathnames (format nil "knowledge-context-~A/" (memory--make-identifier))
                                     (uiop:temporary-directory)))
         (workspace (merge-pathnames "workspace/" root))
         (context   (make-memory-store-context
                     :memory-path    (merge-pathnames "memory.sexp" root)
                     :knowledge-path (merge-pathnames "knowledge.sexp" root)
                     :workspace workspace :mode "active"))
         (service   (make-knowledge-service :context context :session-id "context-check"))
         (checks    0))
    (ensure-directories-exist (merge-pathnames "keep" workspace))
    (unwind-protect
         (labels ((check (value)
                    (incf checks)
                    (unless value (error "Knowledge context check ~D failed." checks)))

                  (fields (&rest pairs)
                    (let ((data (make-hash-table :test #'equal)))
                      (loop for (key value) on pairs by (lambda (items) (rest (rest items)))
                            do (setf (gethash key data) value))
                      data))

                  (recall (turn &optional (active t))
                    (knowledge-context-tests--reply service ':context
                                                    (fields "query" "deploy atlas" "turn" turn "active" active)))

                  (expand (task &rest pairs)
                    (knowledge-context-tests--reply service ':expand
                                                    (apply #'fields "task" task pairs))))
           (knowledge-define-schema
            context (knowledge-make-schema :predicate "project.ready"
                                           :arguments '((:name "project" :type :entity))
                                           :template  "{project} is ready to deploy"))
           (memory-remember context :title "Deploy Atlas" :content "Use the checked workspace build."
                            :scope ':workspace)
           (memory-remember context :title "Deploy Global" :content "Global private guidance."
                            :scope ':global)
           (multiple-value-bind (claim support)
               (knowledge-assert context '("project.ready" "atlas") :basis ':assumption)
             (declare (ignore claim))
             (let ((reply (recall 1)))
               (check (= 1 (length (gethash "memory_lines" reply))))
               (check (= 1 (length (gethash "fact_lines" reply))))
               (check (zerop (gethash "change_count" reply))))
             (knowledge-retract context support :reason "Deployment blocked")
             (let* ((reply     (recall 2))
                    (duplicate (recall 2)))
               (check (plusp (gethash "change_count" reply)))
               (check (equal (gethash "changes" reply) (gethash "changes" duplicate))))
             (check (zerop (gethash "change_count" (recall 3)))))
           (dolist (mode '("passive" "off"))
             (setf (slot-value context 'mode) mode)
             (let ((reply (recall 4)))
               (check (zerop (length (gethash "fact_lines" reply))))
               (check (zerop (gethash "change_count" reply)))
               (check (= 1 (length (gethash "memory_lines" reply))))))
           (setf (slot-value context 'mode) "active")
           (check (zerop (length (gethash "fact_lines" (recall 5 nil)))))
           (knowledge-define-method
            context :id "deploy-atlas" :task #("deploy" "?project")
            :steps (vector (fields "step" "Deploy {?project}")))
           (let* ((reply (expand #("deploy" "atlas")))
                  (steps (gethash "steps" reply)))
             (check (equal (gethash "status" reply) "planned"))
             (check (equal (gethash "text" (aref steps 0)) "Deploy atlas")))
           (check (equal (gethash "status" (expand #("missing"))) "unknown-task"))
           (check (equal (gethash "status" (expand #("deploy" "atlas") "node_limit" 1 "depth_limit" 0))
                         "planned"))
           (knowledge-define-method
            context :id "recursive" :task #("repeat")
            :steps (vector (fields "task" #("repeat"))))
           (check (equal (gethash "status" (expand #("repeat") "node_limit" 2)) "exhausted"))
           (check (handler-case
                      (progn (knowledge-context-tests--reply
                              service ':expand (fields "task" #("deploy" "?project"))) nil)
                    (knowledge-service-error (condition)
                      (eq (knowledge-service-error-code condition) ':invalid-request))))
           (let ((request (knowledge-context-tests--request
                           ':context (fields "query" "atlas" "turn" 6 "active" t))))
             (setf (parent-proto:knowledge-request.scope request) ':global)
             (check (handler-case (progn (knowledge-service-request service request) nil)
                      (knowledge-service-error () t))))
           (let* ((claim (knowledge-assert context '("project.ready" "atlas") :basis ':model-proposal))
                  (reply (recall 7)))
             (declare (ignore claim))
             (check (= 1 (length (gethash "fact_lines" reply))))
             (setf service (make-knowledge-service :context context :session-id "context-check"))
             (check (zerop (gethash "change_count" (recall 8)))))
           checks)
      (dolist (directory (list workspace root))
        (dolist (file (uiop:directory-files directory))
          (delete-file file))
        (when (probe-file directory)
          (uiop:delete-empty-directory directory))))))
