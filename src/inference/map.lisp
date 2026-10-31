(in-package #:autolith)

;;;; -- Parallel Recursive Inference --

(-> rlm-map--normalize-task (t) list)
(defun rlm-map--normalize-task (element)
  "Return ELEMENT as a validated (:task ... :context ...) plist."
  (let ((item (typecase element
                (string
                 (list ':task element))
                (cons
                 element)
                (t
                 nil))))
    (unless (and item (non-empty-string-p (getf item ':task)))
      (error 'rlm-inference-error
             :message
             (format nil "A map element must be a task string or a plist ~
                          with a non-empty :TASK, not ~S." element)))
    item))

(-> rlm-map--run-item
    (list &key (:context t)
               (:contract t)
               (:budget rlm-budget)
               (:capabilities (option keyword))
               (:provider model-provider)
               (:configuration configuration)
               (:source-registry (option tool-registry))
               (:index (integer 0))
               (:total (integer 1))
               (:activity-callback (option function)))
    list)
(defun rlm-map--run-item
    (item &key context contract budget capabilities provider configuration
            source-registry index total activity-callback)
  "Run one map ITEM's frame and return its result or captured failure."
  (let* ((task (getf item ':task))
         (item-activity-callback
           (and activity-callback
                (lambda (activity)
                  (rlm--note-activity
                   activity-callback
                   (format nil "frame ~D/~D · ~A"
                           (1+ index) total activity))))))
    (handler-case
        (multiple-value-bind (value trace-identifier tokens-spent)
            (infer task
                   :model             (getf item ':model)
                   :effort            (getf item ':effort)
                   :context           (append (rlm--context-designators context)
                                              (rlm--context-designators
                                               (getf item ':context)))
                   :contract          contract
                   :budget            budget
                   :capabilities      capabilities
                   :provider          provider
                   :configuration     configuration
                   :source-registry   source-registry
                   :activity-callback item-activity-callback)
          (list ':task task ':value value ':trace trace-identifier
                ':tokens tokens-spent))
      (rlm-partial-result (condition)
        (append (list :task task)
                (rlm-partial-result-observation condition)))
      (error (condition)
        (list ':task task ':error (format nil "~A" condition))))))

(-> rlm-map--job-result (list job) list)
(defun rlm-map--job-result (item job)
  "Await JOB and return its frame outcome, including supervised failures."
  (let ((snapshot (job-await job)))
    (case (getf snapshot :state)
      (:completed
       (getf snapshot :result))
      (otherwise
       (list :task  (getf item :task)
             :error (or (getf snapshot :condition-report)
                        (format nil "Inference frame ~A (~A)."
                                (getf snapshot :state)
                                (getf snapshot :cancellation-reason))))))))

(-> rlm-map--close-pool (job-pool) null)
(defun rlm-map--close-pool (pool)
  "Stop POOL, retaining it in a recoverable condition if shutdown times out."
  (loop
    (when (job-pool-close pool)
      (return nil))
    (restart-case
        (error 'rlm-map-shutdown-error
               :pool    pool
               :message "Inference map workers did not stop before the shutdown deadline.")
      (retry-close ()
        :report "Retry stopping the inference map workers."))))

(-> rlm-map
    (list &key (:context t)
               (:contract t)
               (:budget (option rlm-budget))
               (:capabilities (option keyword))
               (:model (option string))
               (:effort (option string))
               (:provider (option model-provider))
               (:configuration (option configuration))
               (:source-registry (option tool-registry))
               (:concurrency (integer 1))
               (:activity-callback (option function)))
    list)
(defun rlm-map
    (tasks &key context contract budget capabilities model effort provider configuration
                source-registry (concurrency *rlm-map-default-concurrency*)
                activity-callback)
  "Fan TASKS out as inference frames sharing one budget subtree.

TASKS elements are task strings or (:task ... :context ... :model ... :effort ...)
plists whose views are appended to the shared CONTEXT; a task's own model and
effort override the map's. ACTIVITY-CALLBACK receives compact live
frame and request descriptions. Results keep TASKS' order; each is
(:task ... :value ... :trace ... :tokens ...) for a completed frame, with
:tokens carrying the frame's settled billable spend. Budget exhaustion returns
(:task ... :status :incomplete :trace ... :partial-items ...); other failures
return (:task ... :error ...). Finished siblings are never discarded."
  (let ((items (map 'vector #'rlm-map--normalize-task tasks)))
    (when (zerop (length items))
      (return-from rlm-map nil))
    (rlm--note-activity
     activity-callback
     (format nil "starting ~D frame~:P" (length items)))
    (multiple-value-bind (provider configuration)
        (rlm--resolve-environment :model model :effort effort
                                  :provider provider :configuration configuration)
      (let* ((source-registry
               (or source-registry
                   (when (eq capabilities ':read)
                     (rlm--environment-registry))))
             (budget       (or budget (rlm-budget-create)))
             (worker-count (max 1 (min concurrency
                                       *rlm-map-maximum-concurrency*
                                       (length items))))
             (pool         (make-job-pool
                            :name                         "Autolith inference map"
                            :maximum-concurrency          worker-count
                            :maximum-batch-size           (length items)
                            :maximum-live-jobs            (length items)
                            :maximum-runtime-milliseconds 0
                            :terminal-retention-limit     (length items)
                            :start-threads-p              nil)))
        (unwind-protect
             (let ((jobs
                     (job-pool-submit-batch
                      pool
                      (loop for item across items
                            for index from 0
                            collect
                            (let ((item  item)
                                  (index index))
                              (list
                               :name (format nil "Inference frame ~D/~D"
                                             (1+ index) (length items))
                               :function
                               (lambda (job)
                                 (declare (ignore job))
                                 (rlm-map--run-item
                                  item
                                  :context           context
                                  :contract          contract
                                  :budget            budget
                                  :capabilities      capabilities
                                  :provider          provider
                                  :configuration     configuration
                                  :source-registry   source-registry
                                  :index             index
                                  :total             (length items)
                                  :activity-callback activity-callback))))))))
               (loop for item across items
                     for job in jobs
                     collect (rlm-map--job-result item job)))
          (rlm-map--close-pool pool))))))


;;;; -- Shutdown Failure --

(define-condition rlm-map-shutdown-error (rlm-inference-error)
  ((pool
    :initarg       :pool
    :reader        rlm-map-shutdown-error-pool
    :type          job-pool
    :documentation "The map pool still owning workers after bounded shutdown."))
  (:documentation
   "An inference map could not stop its workers; RETRY-CLOSE retries shutdown."))
