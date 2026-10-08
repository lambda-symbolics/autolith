(in-package #:autolith)

;;;; -- Scoped Headless Observation --

(defparameter *run-job-event-root-name* nil
  "The opaque admission name identifying the current headless root job.")

(-> run-job-event--category (t) keyword)
(defun run-job-event--category (category)
  "Return an approved failure CATEGORY without exposing arbitrary symbols."
  (if (member category *run-job-event-categories*)
      category
      ':process-failure))

(-> run-job-event--status (keyword) keyword)
(defun run-job-event--status (status)
  "Translate an accepted pool lifecycle STATUS to a wire status."
  (case status
    (:started ':running)
    (:completed ':succeeded)
    (:aborted ':cancelled)
    (otherwise
     ':failed)))

(-> run-job-event--call-id (string t) (option string))
(defun run-job-event--call-id (run-id call-id)
  "Pseudonymize bounded provider CALL-ID within RUN-ID without retaining a map."
  (when (and (stringp call-id) (<= 1 (length call-id) 4096))
    (ironclad:byte-array-to-hex-string
     (ironclad:digest-sequence
      ':sha256
      (utf8-string-to-octets (format nil "~A:~A" run-id call-id))))))

(-> run-job-event--tool-name (tool-registry t) string)
(defun run-job-event--tool-name (registry name)
  "Return a bounded canonical registry name, or a fixed unknown-tool label."
  (let ((dot (and (stringp name) (<= 1 (length name) 256)
                  (position #\. name))))
    (if (and dot
             (tool-registry-find registry (subseq name 0 dot) (subseq name (1+ dot))))
        name
        "unknown")))

(-> run-job-event--job-directory (configuration session-job) pathname)
(defun run-job-event--job-directory (configuration job)
  "Return JOB's execution-qualified durable artifact directory."
  (merge-pathnames
   (format nil "~A/" (session-job-execution-identifier job))
   (task--artifact-group-root configuration
                              (session-job-root-conversation-identifier job))))

(-> run-job-event--artifact-uri ((or pathname string)) string)
(defun run-job-event--artifact-uri (path)
  "Return a permission-neutral resource reference to a durable artifact PATH."
  (format nil "workspace:~A"
          (workspace-file--encode-identifier
           (uiop:native-namestring (merge-pathnames path)))))

(-> run-job-event--terminal-path (configuration session-job) (option pathname))
(defun run-job-event--terminal-path (configuration job)
  "Return JOB's published execution-qualified terminal artifact, or NIL."
  (let* ((result (job-result job))
         (path (if (typep job 'task-job)
                   (and (listp result) (getf result :output-path))
                   (merge-pathnames
                    "terminal.sexp" (run-job-event--job-directory configuration job)))))
    (when path
      (probe-file path))))

(-> run-job-event--terminal-published-p (configuration session-job) boolean)
(defun run-job-event--terminal-published-p (configuration job)
  "Return whether JOB has published authoritative terminal evidence."
  (not (null (run-job-event--terminal-path configuration job))))

(-> run-job-event--admission-published-p (configuration session-job) boolean)
(defun run-job-event--admission-published-p (configuration job)
  "Return whether JOB has published its execution-qualified admission evidence."
  (not (null (probe-file
              (merge-pathnames
               "continuity.sexp" (run-job-event--job-directory configuration job))))))

(-> run-job-event-observer-create
    (task-orchestrator run-job-event-emitter
     &key (:configuration configuration) (:registry tool-registry)
          (:root-conversation string) (:root-name string))
    function)
(defun run-job-event-observer-create
    (orchestrator emitter &key configuration registry root-conversation root-name)
  "Return a content-free listener scoped to one opaque root and its descendants.

Only the root identifier is retained. Provider call IDs are hashed, arbitrary
text is excluded, and descendant terminals require durable publication. The
root terminal is emitted by RUN-JOB-RUN after its output artifact is published."
  (let ((root-identifier nil)
        (lock (make-lock "headless observer scope"))
        (run-id (run-job-event-emitter-run-id emitter)))
    (labels ((scoped-p (job)
               (with-lock-held (lock)
                 (and (equal root-conversation
                             (session-job-root-conversation-identifier job))
                      (or (and root-identifier
                               (or (equal root-identifier (job-identifier job))
                                   (member root-identifier (job-owner-identifiers job)
                                           :test #'equal)))
                         (and (null root-identifier) (typep job 'task-job)
                               (null (job-owner-identifiers job))
                               (equal root-name (getf (task-job-item job) :name))
                               (progn
                                 (setf root-identifier (job-identifier job))
                                 t))))))

             (root-p (job)
               (with-lock-held (lock)
                 (equal root-identifier (job-identifier job))))

             (emit (kind data)
               (run-job-event-emit emitter kind data))

             (project (channel payload job)
               (let* ((identifier (session-job-execution-identifier job))
                      (base (list :job-id identifier :execution-id identifier))
                      (status (getf payload :status))
                      (call-id (run-job-event--call-id
                                run-id (or (getf payload :call-id)
                                           (getf payload :parent-tool-call-id)))))
                 (case channel
                   (:task-subagent-lifecycle
                    (cond
                      ((eq status :started)
                       (when (run-job-event--admission-published-p configuration job)
                         (emit ':job-started (append base (list :status ':running)))))
                      ((and (not (root-p job))
                            (member status '(:completed :failed :aborted))
                            (run-job-event--terminal-published-p configuration job))
                       (emit ':job-finished
                             (append base
                                     (list :status (run-job-event--status status)
                                           :published-p t
                                           :result-uri
                                           (run-job-event--artifact-uri
                                            (run-job-event--terminal-path configuration job))))))))
                   (:tool-execution-lifecycle
                     (when (and (run-job-event--admission-published-p configuration job)
                                (or (eq status :started)
                                    (and (member status '(:completed :failed :aborted))
                                         (run-job-event--terminal-published-p configuration job))))
                      (emit (if (eq status :started) ':tool-started ':tool-finished)
                            (append base (when call-id (list :call-id call-id))
                                    (list :tool-name
                                          (run-job-event--tool-name registry (getf payload :tool))
                                     :status (run-job-event--status status))
                                    (unless (eq status :started)
                                      (list :result-uri
                                            (run-job-event--artifact-uri
                                             (run-job-event--terminal-path configuration job))))))))
                   (:task-subagent-progress
                    (let ((observed (getf payload :observer-status)))
                      (case observed
                        ((:tool-call-started :tool-call-completed)
                         (emit (if (eq observed :tool-call-started)
                                   ':tool-started ':tool-finished)
                               (append base (when call-id (list :call-id call-id))
                                       (list :tool-name
                                             (run-job-event--tool-name registry (getf payload :tool))
                                             :status
                                             (if (eq observed :tool-call-started)
                                                 ':running
                                                 (if (getf payload :success-p)
                                                     ':succeeded ':failed))))))
                        ((:provider-request-started :provider-request-completed)
                         (emit ':progress
                               (append base (list :status observed)
                                       (when (typep (getf payload :request-count)
                                                    '(integer 0 9223372036854775807))
                                         (list :count (getf payload :request-count)))))
                         (when (eq observed :provider-request-completed)
                           (emit ':usage
                                 (append
                                  (list :job-id identifier)
                                  (loop for key in '(:input-tokens :output-tokens :total-tokens)
                                        for value = (getf payload key)
                                        when (typep value '(integer 0 9223372036854775807))
                                          append (list key value))
                                  (when (typep (getf payload :request-count)
                                               '(integer 0 9223372036854775807))
                                    (list :provider-requests (getf payload :request-count)))))))
                        (otherwise
                         (emit ':progress (append base (list :status ':provider-progress)))))))))))
      (lambda (channel payload)
        (handler-case
            (when (member channel '(:task-subagent-lifecycle :tool-execution-lifecycle
                                    :task-subagent-progress))
              (let ((job (and (stringp (getf payload :id))
                              (task-orchestrator--find-job orchestrator (getf payload :id)))))
                (when (and job (scoped-p job)
                           (run-job-event--admission-published-p configuration job))
                  (project channel payload job))))
          (serious-condition ()
            (run-job-event-emit emitter ':warning (list :code ':invalid-event))))))))
