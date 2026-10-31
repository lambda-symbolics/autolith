(in-package #:autolith)

;;;; -- Asynchronous Inference Frames --

;;; A frame set runs inference frames in the background under one budget
;;; subtree. Its caller submits tasks and receives handles, inspects their
;;; states, collects results as frames finish, and cancels frames whose
;;; answers it no longer needs. RLM-MAP stays the ordered convenience that
;;; waits for every frame.

(defparameter *rlm-frame-set-maximum-frames* 64
  "The most frames one frame set admits over its lifetime.")

(defclass rlm-frame-set ()
  ((pool
    :initarg       :pool
    :reader        rlm-frame-set--pool
    :type          job-pool
    :documentation "The job pool running the set's frames.")
   (arguments
    :initarg       :arguments
    :reader        rlm-frame-set--arguments
    :type          list
    :documentation "The RLM-MAP--RUN-ITEM keyword arguments every frame shares.")
   (completion-function
    :initarg       :completion-function
    :initform      nil
    :reader        rlm-frame-set--completion-function
    :type          (option function)
    :documentation "A function receiving each frame's handle and outcome as it ends.")
   (lock
    :initform      (make-lock "Autolith inference frame set")
    :reader        rlm-frame-set--lock
    :documentation "The lock serializing admission and the handle table.")
   (entries
    :initform      (make-hash-table :test #'equal)
    :reader        rlm-frame-set--entries
    :type          hash-table
    :documentation "Each handle's job and normalized task plist.")
   (count
    :initform      0
    :accessor      rlm-frame-set--count
    :type          (integer 0)
    :documentation "The number of frames admitted so far."))
  (:documentation "Background inference frames addressed by handle."))


;;;; -- Public Functions --

(-> rlm-frame-set-create
    (&key (:context t) (:contract t) (:budget (option rlm-budget))
          (:capabilities (option keyword)) (:model (option string))
          (:effort (option string)) (:provider (option model-provider))
          (:configuration (option configuration))
          (:source-registry (option tool-registry)) (:concurrency (integer 1))
          (:activity-callback (option function))
          (:completion-function (option function)))
    rlm-frame-set)
(defun rlm-frame-set-create
    (&key context contract budget capabilities model effort provider configuration
          source-registry (concurrency *rlm-map-default-concurrency*)
          activity-callback completion-function)
  "Create an empty frame set whose frames share CONTEXT, BUDGET and a provider.

The arguments mean what they mean for RLM-MAP. COMPLETION-FUNCTION receives
each frame's handle and outcome once before terminal publication, including
queued cancellations on the cancelling thread. Close with RLM-FRAME-SET-CLOSE."
  (multiple-value-bind (provider configuration)
      (rlm--resolve-environment :model model :effort effort
                                :provider provider :configuration configuration)
    (make-instance
     'rlm-frame-set
     :pool                (make-job-pool
                           :name                         "Autolith inference frames"
                           :maximum-concurrency          (max 1 (min concurrency
                                                                     *rlm-map-maximum-concurrency*))
                           :maximum-batch-size           *rlm-frame-set-maximum-frames*
                           :maximum-live-jobs            *rlm-frame-set-maximum-frames*
                           :maximum-runtime-milliseconds 0
                           :terminal-retention-limit     *rlm-frame-set-maximum-frames*
                           :start-threads-p              nil)
     :arguments           (list :context           context
                                :contract          contract
                                :budget            (or budget (rlm-budget-create))
                                :capabilities      capabilities
                                :provider          provider
                                :configuration     configuration
                                :source-registry
                                (or source-registry
                                    (when (eq capabilities ':read)
                                      (rlm--environment-registry)))
                                :activity-callback activity-callback)
     :completion-function completion-function)))

(-> rlm-frame-set-submit (rlm-frame-set list &key (:contract t)) list)
(defun rlm-frame-set-submit (set tasks &key contract)
  "Start one background frame per TASKS element and return their handles in order.

Elements are task strings or (:task ... :context ...) plists, as for RLM-MAP.
CONTRACT overrides the set's contract for these frames. Admission is atomic: a
submission past the set's frame limit starts nothing."
  (let ((items (mapcar #'rlm-map--normalize-task tasks)))
    (with-lock-held ((rlm-frame-set--lock set))
      (let* ((start (rlm-frame-set--count set))
             (total (+ start (length items))))
        (when (> total *rlm-frame-set-maximum-frames*)
          (error 'rlm-inference-error
                 :message (format nil "A frame set runs at most ~D frames."
                                  *rlm-frame-set-maximum-frames*)))
        (let* ((handles (loop for index from (1+ start) to total
                              collect (format nil "frame-~D" index)))
               (jobs
                 (job-pool-submit-batch
                  (rlm-frame-set--pool set)
                  (loop for item in items
                        for handle in handles
                        for index from start
                        collect (rlm-frame-set--admission
                                 set item handle
                                 :index index :total total :contract contract)))))
          (loop for handle in handles
                for item in items
                for job in jobs
                do (setf (gethash handle (rlm-frame-set--entries set))
                         (list :job job :item item)))
          (setf (rlm-frame-set--count set) total)
          handles)))))

(-> rlm-frame-set-status (rlm-frame-set list) list)
(defun rlm-frame-set-status (set handles)
  "Return (:handle ... :task ... :state ...) for each of HANDLES.

States are :queued, :running, :completed, :failed and :aborted."
  (mapcar (lambda (handle)
            (let ((entry (rlm-frame-set--find set handle)))
              (list :handle handle
                    :task   (getf (getf entry :item) :task)
                    :state  (getf (job-snapshot (getf entry :job)) :state))))
          handles))

(-> rlm-frame-set-collect
    (rlm-frame-set list &key (:timeout-seconds (option (real 0))))
    list)
(defun rlm-frame-set-collect (set handles &key timeout-seconds)
  "Return the results of the finished frames among HANDLES, in HANDLES order.

When none has finished, wait until one does or TIMEOUT-SECONDS pass; NIL waits
without limit, and a timeout returns NIL. A result has RLM-MAP's shape with a
leading :HANDLE. Collecting a frame again returns the same result."
  (let ((entries (mapcar (lambda (handle) (rlm-frame-set--find set handle)) handles)))
    (job-await-any (mapcar (lambda (entry) (getf entry :job)) entries)
                   :timeout-seconds timeout-seconds)
    (loop for handle in handles
          for entry in entries
          when (job-terminal-p (getf entry :job))
            collect (list* :handle handle
                           (rlm-map--job-result (getf entry :item) (getf entry :job))))))

(-> rlm-frame-set-cancel (rlm-frame-set list) list)
(defun rlm-frame-set-cancel (set handles)
  "Cancel the unfinished frames among HANDLES and return their states.

A cancelled frame's in-flight request counts as spent against the budget,
since the provider may already bill it. A running frame can still show
:RUNNING in the returned states while it unwinds."
  (dolist (handle handles)
    (job-cancel (getf (rlm-frame-set--find set handle) :job) :reason ':cancelled))
  (rlm-frame-set-status set handles))

(-> rlm-frame-set-close (rlm-frame-set) null)
(defun rlm-frame-set-close (set)
  "Cancel every unfinished frame of SET, then stop its workers."
  (let ((jobs (with-lock-held ((rlm-frame-set--lock set))
                (loop for entry being the hash-values of (rlm-frame-set--entries set)
                      collect (getf entry :job)))))
    (dolist (job jobs)
      (job-cancel job :reason ':closed)))
  (rlm-map--close-pool (rlm-frame-set--pool set)))


;;;; -- Private Functions --

(-> rlm-frame-set--find (rlm-frame-set t) list)
(defun rlm-frame-set--find (set handle)
  "Return HANDLE's entry in SET, or signal an inference error naming it."
  (or (and (stringp handle)
           (with-lock-held ((rlm-frame-set--lock set))
             (gethash handle (rlm-frame-set--entries set))))
      (error 'rlm-inference-error
             :message (format nil "No inference frame has the handle ~S." handle))))

(-> rlm-frame-set--admission
    (rlm-frame-set list string &key (:index (integer 0)) (:total (integer 1)) (:contract t))
    list)
(defun rlm-frame-set--admission (set item handle &key index total contract)
  "Return the job admission entry running ITEM as frame HANDLE.

The terminal hook reports the resolved outcome exactly once, including queued
cancellation and cancellation racing with completion. The worker's abort handler
keeps in-flight provider reservations charged while the frame unwinds."
  (let ((arguments           (rlm-frame-set--arguments set))
        (completion-function (rlm-frame-set--completion-function set)))
    (list :name (format nil "Inference ~A" handle)
          :terminal-result-function
          (lambda (job state result report)
            (declare (ignore job))
            (when completion-function
              (handler-case
                  (funcall completion-function handle
                           (case state
                             (:completed
                              result)
                             (:aborted
                              (list :task (getf item :task) :status ':cancelled))
                             (otherwise
                              (list :task (getf item :task) :error report))))
                (error (condition)
                  (format *error-output* "~&The inference completion function failed: ~A~%"
                          condition))))
            (values result report state))
          :function
          (lambda (job)
            (declare (ignore job))
            (let ((*rlm-frame-cancelled-p* nil))
              (handler-bind
                  ((job-aborted
                     (lambda (condition)
                       (declare (ignore condition))
                       (setf *rlm-frame-cancelled-p* t))))
                (apply #'rlm-map--run-item item
                       :index    index
                       :total    total
                       :contract (or contract (getf arguments :contract))
                       arguments)))))))
