(in-package #:autolith)

;;;; -- Parallel Recursive Inference --

(defparameter *rlm-map-default-concurrency* 4
  "The default number of frames one RLM-MAP runs concurrently.")

(defparameter *rlm-map-maximum-concurrency* 8
  "The largest supported RLM-MAP worker pool.")

(-> rlm-map--normalize-task (t) list)
(defun rlm-map--normalize-task (element)
  "Return ELEMENT as a validated (:task ... :context ...) plist."
  (let ((item (typecase element
                (string (list ':task element))
                (cons element)
                (t nil))))
    (unless (and item (non-empty-string-p (getf item ':task)))
      (error 'rlm-inference-error
             :message
             (format nil "A map element must be a task string or a plist ~
                          with a non-empty :TASK, not ~S." element)))
    item))

(-> rlm-map--run-item
    (list list t rlm-budget (option keyword) model-provider configuration
     (option tool-registry))
    list)
(defun rlm-map--run-item
    (item context contract budget capabilities provider configuration
     source-registry)
  "Run one map ITEM's frame and return its result or captured failure."
  (let ((task (getf item ':task)))
    (handler-case
        (multiple-value-bind (value trace-identifier)
            (infer task
                   :context (append context (getf item ':context))
                   :contract contract
                   :budget budget
                   :capabilities capabilities
                   :provider provider
                   :configuration configuration
                   :source-registry source-registry)
          (list ':task task ':value value ':trace trace-identifier))
      (error (condition)
        (list ':task task ':error (format nil "~A" condition))))))

(-> rlm-map
    (list &key (:context list)
               (:contract t)
               (:budget (option rlm-budget))
               (:capabilities (option keyword))
               (:provider (option model-provider))
               (:configuration (option configuration))
               (:source-registry (option tool-registry))
               (:concurrency (integer 1)))
    list)
(defun rlm-map
    (tasks &key context contract budget capabilities provider configuration
                source-registry (concurrency *rlm-map-default-concurrency*))
  "Fan TASKS out as inference frames sharing one budget subtree.

TASKS elements are task strings or (:task ... :context ...) plists
whose views are appended to the shared CONTEXT. Results keep TASKS'
order; each is (:task ... :value ... :trace ...) for a completed
frame or (:task ... :error ...) for one that failed, so exhausting
the shared BUDGET fails the remaining frames without discarding the
finished ones."
  (let ((items (map 'vector #'rlm-map--normalize-task tasks)))
    (when (zerop (length items))
      (return-from rlm-map nil))
    (multiple-value-bind (environment-provider environment-configuration)
        (if (and provider configuration)
            (values provider configuration)
            (rlm--environment))
      (let* ((provider (or provider environment-provider))
             (configuration (or configuration environment-configuration))
             (source-registry
               (or source-registry
                   (when (eq capabilities ':read)
                     (rlm--environment-registry))))
             (budget (or budget (rlm-budget-create)))
             (results (make-array (length items) :initial-element nil))
             (next 0)
             (claim-lock (make-lock "Autolith inference map"))
             (worker-count (max 1 (min concurrency
                                       *rlm-map-maximum-concurrency*
                                       (length items)))))
        (flet ((work ()
                 (loop
                   (let ((index
                           (with-lock-held (claim-lock)
                             (when (< next (length items))
                               (prog1 next (incf next))))))
                     (unless index
                       (return))
                     (setf (aref results index)
                           (rlm-map--run-item
                            (aref items index) context contract budget
                            capabilities provider configuration
                            source-registry))))))
          (if (= worker-count 1)
              (work)
              (mapc #'join-thread
                    (loop repeat worker-count
                          collect (make-thread #'work
                                               :name "autolith-rlm-map")))))
        (coerce results 'list)))))
