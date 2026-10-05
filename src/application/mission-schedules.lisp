(in-package #:autolith)

;;;; -- Durable Mission Wakeups --

(defclass mission-schedule-service ()
  ((application :initarg :application :reader mission-schedule-application
                :documentation "Primary application owning this scheduler.")
   (conversation :initarg :conversation :reader mission-schedule-conversation
                 :documentation "Root conversation captured when the store was selected.")
   (scheduler :accessor mission-schedule-scheduler
              :documentation "Library scheduler with durable claim history.")
   (retired-p :initform nil :accessor mission-schedule-retired-p
              :documentation "Withdrawn service identity after atomic reconstruction or rekeying.")
   (lock :initform (make-recursive-lock "Mission wakeups") :reader mission-schedule-lock
         :documentation "Serializes admission, execution and explicit recovery.")
   (notify :initarg :notify :initform nil :accessor mission-schedule-notify
           :documentation "Wake the existing application event loop after deadline changes.")
   (executing :initform (make-hash-table :test #'equal) :reader mission-schedule-executing
              :documentation "Live reservations, excluding concurrent explicit retry decisions.")
   (enqueue :initarg :enqueue :reader mission-schedule-enqueue
            :documentation "Admit a command to the ordinary application input queue.")
   (run :initarg :run :reader mission-schedule-run
        :documentation "Execute admitted mission work through ordinary continuation policy."))
  (:documentation "Product authority and admission around a durable library scheduler."))

(defvar *mission-schedule-services* (make-hash-table :test #'eq :weakness ':key)
  "Schedulers keyed by their primary application identity.")
(defvar *mission-schedule-services-lock* (make-lock "Mission schedule services")
  "Protects creation and lookup of application schedulers.")

(defparameter *mission-schedule-snapshot-byte-limit* (* 4 1024 1024)
  "Shared byte bound for admission, durable publication and restoration.")

(-> mission-schedule--read (pathname) t)
(defun mission-schedule--read (path)
  "Read a bounded portable snapshot, refusing incomplete records."
  (when (probe-file path)
    (multiple-value-bind (record complete-p)
        (snapshot-read path :grammar (task--result-grammar)
                            :maximum-octets *mission-schedule-snapshot-byte-limit*)
      (unless complete-p (mission--reject ':schedule-storage "Incomplete schedule snapshot."))
      record)))

(-> mission-schedule--write (pathname t) pathname)
(defun mission-schedule--write (path record)
  "Atomically publish RECORD before changing visible scheduler state."
  (let ((text (format nil "~A~%" (task--write-readable-sexp record))))
    (when (> (length (utf8-string-to-octets text)) *mission-schedule-snapshot-byte-limit*)
      (mission--reject ':schedule-storage "The schedule snapshot exceeds its restoration byte bound."))
    (ensure-directories-exist path)
    (snapshot-write-text path text)))

(-> mission-schedule--queue (application string) boolean)
(defun mission-schedule--queue (application command)
  "Admit COMMAND through the existing primary input queue without steering a turn."
  (let ((controller (application-input-controller application)))
    (when (and controller (application-input-controller--enqueue controller ':command command))
      (application-input-controller--persist-pending controller)
      t)))

(-> mission-schedule--run (application string) t)
(defun mission-schedule--run (application content)
  "Run CONTENT as a mission continuation, preserving ordinary mission budget checks."
  (flet ((run (take acknowledge)
           (application--run-turn application content :continuation-p t
                                  :steering-function take
                                  :steering-persisted-function acknowledge)))
    (let ((controller (application-input-controller application)))
      (if controller
          (application-input-controller-call-with-primary-steering controller #'run)
          (run (lambda () (peer-message-take-context (application-agent application)))
               (lambda (identifier)
                 (peer-message-ack-context (application-agent application) identifier)))))))

(-> mission-schedule-service-create
    (application &key (:path (option pathname)) (:clock function)
                 (:notify (option function)) (:enqueue function) (:run function)) mission-schedule-service)
(defun mission-schedule-service-create
    (application &key path (clock #'get-universal-time) notify
                 (enqueue #'mission-schedule--queue) (run #'mission-schedule--run))
  "Restore APPLICATION's scheduler; interrupted claims become explicit UNKNOWN outcomes."
  (let* ((path (or path (make-pathname :type "mission-schedules.sexp"
                                     :defaults (conversation-pathname (application-conversation application)))))
         (service (make-instance 'mission-schedule-service :application application
                                 :conversation (conversation-identifier (application-conversation application))
                                 :notify notify :enqueue enqueue :run run)))
    (setf (mission-schedule-scheduler service)
          (cl-jobpond:make-scheduler :clock clock :snapshot (mission-schedule--read path)
                                    :capacity 1024
                                    :store (lambda (record) (mission-schedule--write path record))))
    service))

(-> application-mission-schedules (application) mission-schedule-service)
(defun application-mission-schedules (application)
  "Return APPLICATION's unique primary scheduler, restoring durable state once."
  (with-lock-held (*mission-schedule-services-lock*)
    (let ((service (gethash application *mission-schedule-services*)))
      (if (and service (equal (mission-schedule-conversation service)
                              (conversation-identifier (application-conversation application))))
          service
          (flet ((replace-service ()
                   (let ((replacement
                           (mission-schedule-service-create
                            application :notify (and service (mission-schedule-notify service)))))
                     (when service (setf (mission-schedule-retired-p service) t))
                     (setf (gethash application *mission-schedule-services*) replacement))))
            (if service
                (with-recursive-lock-held ((mission-schedule-lock service))
                  (mission-schedule--require-quiescent service)
                  (replace-service))
                (replace-service)))))))

(-> mission-schedule--require-active (mission-schedule-service) null)
(defun mission-schedule--require-active (service)
  "Refuse operations holding an obsolete service across reconstruction."
  (when (mission-schedule-retired-p service)
    (mission--reject ':schedule-retired "The scheduler service was reconstructed."))
  nil)

(-> mission-schedule--require-quiescent (mission-schedule-service) null)
(defun mission-schedule--require-quiescent (service)
  "Refuse to replace SERVICE while an admitted operation can still produce effects."
  (when (plusp (hash-table-count (mission-schedule-executing service)))
    (mission--reject ':schedule-executing "A live wakeup prevents scheduler reconstruction."))
  nil)

(-> application-mission-schedules-recover (application &key (:startup-p boolean)) null)
(defun application-mission-schedules-recover (application &key startup-p)
  "Restore uncertain claims, discarding saved reservations only at process startup."
  (with-lock-held (*mission-schedule-services-lock*)
    (let ((previous (gethash application *mission-schedule-services*)))
      (flet ((restore ()
               (let ((replacement
                       (mission-schedule-service-create
                        application :notify (and previous (mission-schedule-notify previous)))))
                 (when previous (setf (mission-schedule-retired-p previous) t))
                 (setf (gethash application *mission-schedule-services*) replacement))))
        (if previous
            (with-recursive-lock-held ((mission-schedule-lock previous))
              (unless startup-p (mission-schedule--require-quiescent previous))
              (restore))
            (restore)))))
  nil)


(-> application-mission-schedules-notify (application) null)
(defun application-mission-schedules-notify (application)
  "Nudge a hosted scheduler after reconstruction, outside enclosing application/render locks."
  (let ((service (with-lock-held (*mission-schedule-services-lock*)
                   (gethash application *mission-schedule-services*))))
    (when service (mission-schedule--notify service)))
  nil)

(-> mission-schedule--notify (mission-schedule-service) null)
(defun mission-schedule--notify (service)
  "Notify the existing host event loop without running a separate scheduler thread."
  (when (mission-schedule-notify service) (funcall (mission-schedule-notify service)))
  nil)

(-> mission-schedule--identity (application) list)
(defun mission-schedule--identity (application)
  "Install durable identity/version fields on the exact current mission when needed."
  (let ((goal (application-goal application))
        (context (mission-context-find (application-conversation application))))
    (unless (and context (eq goal (mission-context-goal context))
                 (mission-goal-p goal) (eq (getf goal :status) ':active))
      (mission--reject ':schedule-mission "Schedules require the current active mission."))
    (with-recursive-lock-held ((mission-context-lock context))
      (unless (and (stringp (getf goal :schedule-identity)) (stringp (getf goal :schedule-version)))
        (when (or (getf goal :schedule-identity) (getf goal :schedule-version))
          (mission--reject ':schedule-identity "The mission scheduling identity is incomplete."))
        ;; Preserve the exact goal cons identity shared by admitted mission work.
        (nconc goal (list :schedule-identity (make-identifier) :schedule-version (make-identifier)))
        (mission--record context))
      (list :mission (getf goal :schedule-identity) :version (getf goal :schedule-version)))))

(-> mission-schedule--current-p (mission-schedule-service list) boolean)
(defun mission-schedule--current-p (service payload)
  "Require exact mission identity/version and active state at admission and execution."
  (let ((goal (application-goal (mission-schedule-application service))))
    (not (null (and (mission-goal-p goal) (eq (getf goal :status) ':active)
                    (equal (getf goal :schedule-identity) (getf payload :mission))
                    (equal (getf goal :schedule-version) (getf payload :version)))))))

(-> application-mission-schedule-add
    (application &key (:id string) (:content string) (:at (option integer))
                 (:interval (option integer)) (:event (option string)) (:missed-policy keyword)) list)
(defun application-mission-schedule-add
    (application &key id content at interval event (missed-policy ':latest))
  "Attach an immutable explicit wakeup to the current mission version."
  (unless (and (non-empty-string-p content) (<= (length content) 8192))
    (mission--reject ':schedule-payload "Wakeup content must contain 1 to 8192 characters."))
  (let ((service (application-mission-schedules application)))
    (prog1
        (with-recursive-lock-held ((mission-schedule-lock service))
          (mission-schedule--require-active service)
          (let* ((identity (mission-schedule--identity application))
                 (payload (append identity (list :content content))))
            (cl-jobpond:scheduler-add (mission-schedule-scheduler service)
                                      :id id :version (getf identity :version) :payload payload
                                      :at at :interval interval :event event :missed-policy missed-policy)))
      (mission-schedule--notify service))))

(-> mission-schedule--settle (mission-schedule-service list &key (:outcome keyword) (:result t)) list)
(defun mission-schedule--settle (service wakeup &key outcome result)
  "Durably settle WAKEUP with its exact claim token."
  (cl-jobpond:scheduler-ack (mission-schedule-scheduler service)
                           :id (getf wakeup :id) :token (getf wakeup :token)
                           :outcome outcome :result result))

(-> mission-schedule--admit (mission-schedule-service) list)
(defun mission-schedule--admit (service)
  "Claim pending occurrences before admission; never repeat an uncertain admission."
  (mission-schedule--require-active service)
  (let ((scheduler (mission-schedule-scheduler service)) (results nil))
    (dolist (pending (getf (cl-jobpond:scheduler-snapshot scheduler) :wakeups))
      (when (eq (getf pending :state) ':pending)
        (let ((wakeup (cl-jobpond:scheduler-claim scheduler :id (getf pending :id)
                                                :version (getf pending :version))))
          (cond
            ((not (mission-schedule--current-p service (getf wakeup :payload)))
             (cl-jobpond:scheduler-cancel scheduler :id (getf wakeup :schedule) :version (getf wakeup :version))
             (push (mission-schedule--settle service wakeup :outcome ':cancelled :result ':stale-mission) results))
            (t
             (handler-case
                 (let ((accepted-p
                         (funcall (mission-schedule-enqueue service)
                                  (mission-schedule-application service)
                                  (format nil "/mission-wakeup ~A"
                                          (task--write-readable-sexp
                                           (list :id (getf wakeup :id) :token (getf wakeup :token)))))))
                   (push (if accepted-p wakeup
                             (mission-schedule--settle service wakeup :outcome ':failed :result ':admission-refused)) results))
               (error ()
                 (push (cl-jobpond:scheduler-resolve scheduler :id (getf wakeup :id)
                                                    :token (getf wakeup :token) :action ':unknown) results))))))))
    (nreverse results)))

(-> application-mission-schedules-wake (application &key (:now (option integer))) list)
(defun application-mission-schedules-wake (application &key now)
  "Materialize due times and enqueue only proven current pending mission work."
  (let ((service (application-mission-schedules application)))
    (with-recursive-lock-held ((mission-schedule-lock service))
      (mission-schedule--require-active service)
      (cl-jobpond:scheduler-wake (mission-schedule-scheduler service) :now now)
      (mission-schedule--admit service))))

(-> application-mission-schedule-event (application string string &key (:payload t)) list)
(defun application-mission-schedule-event (application event id &key payload)
  "Deliver one stable external event and admit its current mission occurrences."
  (let ((service (application-mission-schedules application)))
    (prog1 (with-recursive-lock-held ((mission-schedule-lock service))
             (mission-schedule--require-active service)
             (cl-jobpond:scheduler-event (mission-schedule-scheduler service) :event event :id id :payload payload)
             (mission-schedule--admit service))
      (mission-schedule--notify service))))

(-> application-mission-schedule-cancel (application string) list)
(defun application-mission-schedule-cancel (application id)
  "Cancel future/pending occurrences without erasing uncertain claims or dedup history."
  (let ((service (application-mission-schedules application)))
    (prog1 (with-recursive-lock-held ((mission-schedule-lock service))
             (mission-schedule--require-active service)
             (cl-jobpond:scheduler-cancel (mission-schedule-scheduler service) :id id))
      (mission-schedule--notify service))))

(-> application-mission-schedules-next-time (application) (option integer))
(defun application-mission-schedules-next-time (application)
  "Return the next absolute deadline for the existing host event loop."
  (let ((scheduler (mission-schedule-scheduler (application-mission-schedules application))))
    (if (find ':pending (getf (cl-jobpond:scheduler-snapshot scheduler) :wakeups)
              :key (lambda (entry) (getf entry :state)))
        (get-universal-time)
        (cl-jobpond:scheduler-next-time scheduler))))

(-> application-mission-schedule-execute (application list) t)
(defun application-mission-schedule-execute (application ticket)
  "Execute an admitted ticket only once and recheck mission version at consumption."
  (let* ((service (application-mission-schedules application))
         (scheduler (mission-schedule-scheduler service))
         (wakeup nil))
    (with-recursive-lock-held ((mission-schedule-lock service))
      (mission-schedule--require-active service)
      (setf wakeup (find (getf ticket :id) (getf (cl-jobpond:scheduler-snapshot scheduler) :wakeups)
                         :key (lambda (entry) (getf entry :id)) :test #'equal))
      (unless (and wakeup (eq (getf wakeup :state) ':claimed)
                   (eql (getf ticket :token) (getf wakeup :token)))
        (mission--reject ':schedule-ticket "Wakeup ticket is stale or requires explicit recovery."))
      (when (or (getf wakeup :cancelled)
                (not (mission-schedule--current-p service (getf wakeup :payload))))
        (return-from application-mission-schedule-execute
          (mission-schedule--settle service wakeup :outcome ':cancelled :result ':stale-mission)))
      ;; Durable UNKNOWN prevents implicit replay after a crash or a copied command.
      (cl-jobpond:scheduler-resolve scheduler :id (getf wakeup :id)
                                            :token (getf wakeup :token) :action ':unknown)
      (setf (gethash (getf wakeup :id) (mission-schedule-executing service)) t))
    (unwind-protect
         (progn
           (funcall (mission-schedule-run service) application (getf (getf wakeup :payload) :content))
           (with-recursive-lock-held ((mission-schedule-lock service))
             (mission-schedule--settle service wakeup :outcome ':completed :result ':executed)))
      (with-recursive-lock-held ((mission-schedule-lock service))
        (remhash (getf wakeup :id) (mission-schedule-executing service))))))

(-> application-mission-schedule-resolve
    (application list &key (:token integer) (:action keyword)) list)
(defun application-mission-schedule-resolve (application id &key token action)
  "Explicitly resolve an interrupted claim as completed/failed or authorize retry."
  (let ((service (application-mission-schedules application)))
    (prog1
        (with-recursive-lock-held ((mission-schedule-lock service))
          (mission-schedule--require-active service)
          (when (gethash id (mission-schedule-executing service))
            (mission--reject ':schedule-executing "A live wakeup cannot be resolved or retried concurrently."))
          (case action
            ((:completed :failed)
             (cl-jobpond:scheduler-ack (mission-schedule-scheduler service)
                                      :id id :token token :outcome action :result ':primary-resolution))
            (:retry
             (cl-jobpond:scheduler-resolve (mission-schedule-scheduler service)
                                          :id id :token token :action ':retry))
            (otherwise (mission--reject ':schedule-resolution "Expected completed, failed or retry."))))
      (mission-schedule--notify service))))

(-> application-mission-wakeup-command (application string) null)
(defun application-mission-wakeup-command (application remainder)
  "Consume a persisted scheduler ticket through the ordinary local command queue."
  (application-mission-schedule-execute application (mission--read-data remainder))
  nil)
