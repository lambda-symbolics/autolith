(in-package #:autolith)

;;;; -- Recursive Inference Policies --

(defgeneric rlm-decompose-inference-task (policy task views budget)
  (:documentation
   "Return RLM-MAP subtask plists for TASK under POLICY, or NIL to run it directly.

Methods are eql-specialized on POLICY keywords so live self
modification can promote decomposition strategies distilled from
successful traces. VIEWS are TASK's materialized context views; each
returned subtask is a (:task ... :context ...) plist and the whole
fan-out runs under TASK's shared BUDGET."))

(defmethod rlm-decompose-inference-task
    ((policy (eql ':direct)) (task string) (views list) (budget rlm-budget))
  "Never decompose: the default policy runs TASK as one frame."
  (declare (ignore task views budget))
  nil)

(defgeneric rlm-synthesize-inference-results
    (policy task results
     &key contract budget capabilities provider configuration source-registry)
  (:documentation
   "Compose TASK's answer from its RLM-MAP subtask RESULTS under POLICY.

Methods are eql-specialized on POLICY keywords like the decomposition
side. The default synthesis runs one more frame whose views are the
subtask results, so failed subtasks stay visible instead of silently
vanishing. Returns the value and its trace identifier."))

(-> rlm--synthesis-views (list) list)
(defun rlm--synthesis-views (results)
  "Convert RLM-MAP RESULTS into labeled synthesis views."
  (loop for result in results
        for index from 1
        collect
        (list ':label (format nil "subtask ~D" index)
              ':content
              (format nil "Task: ~A~%~A"
                      (getf result ':task)
                      (cond
                        ((getf result ':error)
                         (format nil "Failed: ~A" (getf result ':error)))
                        ((stringp (getf result ':value))
                         (getf result ':value))
                        (t
                         (rlm--result-sexp (getf result ':value))))))))

(defmethod rlm-synthesize-inference-results
    ((policy t) (task string) (results list)
     &key contract budget capabilities provider configuration
          source-registry)
  "Synthesize by one frame reading every subtask result as a view.

This default applies to every policy, so a promoted strategy may
override decomposition alone and inherit the synthesis frame."
  (infer (format nil
                 "Subtask results are attached as views. Compose the answer to this task from them: ~A"
                 task)
         :context (rlm--synthesis-views results)
         :contract contract
         :budget budget
         :capabilities capabilities
         :provider provider
         :configuration configuration
         :source-registry source-registry))

(-> rlm-run
    (string &key (:policy keyword)
                 (:context list)
                 (:contract t)
                 (:budget (option rlm-budget))
                 (:capabilities (option keyword))
                 (:model (option string))
                 (:effort (option string))
                 (:provider (option model-provider))
                 (:configuration (option configuration))
                 (:source-registry (option tool-registry))
                 (:concurrency (integer 1)))
    (values t string))
(defun rlm-run
    (task &key (policy ':direct) context contract budget capabilities model
               effort provider configuration source-registry
               (concurrency *rlm-map-default-concurrency*))
  "Run TASK under POLICY: decompose, fan subtasks out, and synthesize.

When POLICY declines to decompose, TASK runs as one inference frame.
Otherwise the subtasks run as text frames through RLM-MAP and the
policy's synthesis composes the final answer under CONTRACT, all
sharing one BUDGET subtree."
  (multiple-value-bind (provider configuration)
      (rlm--resolve-environment :model model :effort effort
                                :provider provider
                                :configuration configuration)
    (let* ((views (rlm-views-materialize context))
           (budget (or budget (rlm-budget-create)))
           (subtasks
             (rlm-decompose-inference-task policy task views budget)))
      (if (null subtasks)
          (infer task
                 :context views
                 :contract contract
                 :budget budget
                 :capabilities capabilities
                 :provider provider
                 :configuration configuration
                 :source-registry source-registry)
          (rlm-synthesize-inference-results
           policy task
           (rlm-map subtasks
                    :budget budget
                    :capabilities capabilities
                    :provider provider
                    :configuration configuration
                    :source-registry source-registry
                    :concurrency concurrency)
           :contract contract
           :budget budget
           :capabilities capabilities
           :provider provider
           :configuration configuration
           :source-registry source-registry)))))
