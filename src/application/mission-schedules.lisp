(in-package #:autolith)

;;;; -- Durable Mission Wakeups --

(defstruct mission-schedule-outcome
  "Terminal runner result; only fixed reason categories may enter durable history."
  (state ':unknown :type (member :completed :failed :unknown))
  (reason ':interrupted :type (member :executed :provider-failed :runner-failed :interrupted)))

(defvar *mission-schedule-notice-lock* (make-recursive-lock "Mission occurrence notices")
  "Serialize durable occurrence lookup, append and conversation replacement.")

(defmethod (setf application-conversation) :around (conversation (application application))
  "Keep occurrence presentation and conversation replacement in separate critical sections."
  (with-recursive-lock-held (*mission-schedule-notice-lock*)
    (call-next-method)))

(defclass mission-schedule-service ()
  ((application :initarg :application :reader mission-schedule-application
                :documentation "Primary application owning this scheduler.")
   (conversation :initarg :conversation :reader mission-schedule-conversation
                 :documentation "Root conversation captured when the store was selected.")
   (notice-conversation :initarg :notice-conversation :reader mission-schedule-notice-conversation
                        :documentation "Owning conversation object for durable occurrence notices.")
   (scheduler :accessor      mission-schedule-scheduler
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
        (snapshot-read path :grammar        (task--result-grammar)
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
(defun mission-schedule--queue (application form)
  "Admit a native wakeup FORM noninteractively through the primary input queue."
  (let ((controller (application-input-controller application)))
    (when (and controller (application-input-controller--enqueue controller ':native-lisp form))
      (application-input-controller--persist-pending controller)
      t)))

(-> mission-schedule--run (application string) t)
(defun mission-schedule--run (application content)
  "Run CONTENT as a mission continuation, preserving ordinary mission budget checks."
  (flet ((run (take acknowledge)
           (let ((result (nth-value 1 (application--run-turn
                                       application content :continuation-p t
                                       :steering-function take :steering-persisted-function acknowledge))))
             (cond
               ((and (typep result 'provider-result)
                     (agent-turn-complete-p (application-agent application) result))
                (make-mission-schedule-outcome :state ':completed :reason ':executed))
               ((eq result ':failed)
                (make-mission-schedule-outcome :state ':failed :reason ':runner-failed))
               (t
                (make-mission-schedule-outcome))))))
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
  (let* ((conversation (application-conversation application))
         (path         (or path (make-pathname :type    "mission-schedules.sexp"
                                              :defaults (conversation-pathname conversation))))
         (service      (make-instance 'mission-schedule-service :application application
                                     :conversation                           (conversation-identifier conversation)
                                     :notice-conversation                    conversation
                                     :notify notify :enqueue enqueue :run run)))
    (setf (mission-schedule-scheduler service)
          (cl-jobpond:make-scheduler :clock clock :snapshot (mission-schedule--read path)
                                    :capacity 1024
                                    :store    (lambda (record) (mission-schedule--write path record))))
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
  (mission-schedule--announce (application-mission-schedules application))
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
  (let ((goal    (application-goal application))
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

(-> mission-schedule--disposition (mission-schedule-service list) keyword)
(defun mission-schedule--disposition (service payload)
  "Separate obsolete mission authority from temporarily unavailable mission work."
  (let ((goal (application-goal (mission-schedule-application service))))
    (cond
      ((or (not (mission-goal-p goal))
           (not (equal (getf goal :schedule-identity) (getf payload :mission)))
           (not (equal (getf goal :schedule-version) (getf payload :version)))
           (member (getf goal :status) '(:verified :exhausted :cancelled :failed :complete)))
       ':stale)
      ((eq (getf goal :status) ':active)
       ':runnable)
      (t
       ':suspended))))

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
                 (payload  (append identity (list :content content))))
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

(-> mission-schedule--withhold (mission-schedule-service list keyword) list)
(defun mission-schedule--withhold (service wakeup disposition)
  "Settle unavailable work without withdrawing a suspended mission's schedule."
  (when (eq disposition ':stale)
    (cl-jobpond:scheduler-cancel (mission-schedule-scheduler service)
                                :id (getf wakeup :schedule) :version (getf wakeup :version)))
  ;; The scheduler's acknowledgement states do not include SKIPPED.
  (mission-schedule--settle
   service wakeup :outcome ':cancelled
   :result                 (if (eq disposition ':stale) ':stale-mission
                               (list :skipped (getf (application-goal (mission-schedule-application service)) :status)))))

(-> mission-schedule--present-outcomes (application list) list)
(defun mission-schedule--present-outcomes (application wakeups)
  "Persist and display skipped or cancelled outcomes outside the schedule lock."
  (dolist (wakeup wakeups)
    (let ((result (getf wakeup :result)))
      (when (or (eq result ':stale-mission)
                (and (consp result) (eq (first result) ':skipped)))
        (application-side-status-present
         application "mission schedule"
         (list (terminal-span ':notice
                (format nil "Mission schedule ~A occurrence ~S: ~A (~A)."
                        (getf wakeup :schedule) (getf wakeup :id)
                        (if (eq result ':stale-mission) "cancelled" "skipped")
                        (if (eq result ':stale-mission) "stale mission" (second result)))))))))
  wakeups)

(-> mission-schedule--admit (mission-schedule-service) list)
(defun mission-schedule--admit (service)
  "Claim pending occurrences before admission; never repeat an uncertain admission."
  (mission-schedule--require-active service)
  (let ((scheduler (mission-schedule-scheduler service)) (results nil))
    ;; Re-read pending work: cancellation can settle siblings from an earlier snapshot.
    (loop for pending = (find ':pending (getf (cl-jobpond:scheduler-snapshot scheduler) :wakeups)
                             :key (lambda (entry) (getf entry :state)))
          while pending
          do (let* ((wakeup      (cl-jobpond:scheduler-claim scheduler :id (getf pending :id)
                                                          :version         (getf pending :version)))
                    (disposition (mission-schedule--disposition service (getf wakeup :payload))))
               (cond
                 ((not (eq disposition ':runnable))
                  (push (mission-schedule--withhold service wakeup disposition) results))
                 (t
                  (handler-case
                      (let ((accepted-p
                              (funcall (mission-schedule-enqueue service)
                                       (mission-schedule-application service)
                                       (task--write-readable-sexp
                                        (list 'mission-wakeup
                                              (list 'quote
                                                    (list :id (getf wakeup :id) :token (getf wakeup :token))))))))
                        (push (if accepted-p wakeup
                                  (mission-schedule--settle service wakeup :outcome ':failed :result ':admission-refused)) results))
                    (error ()
                      (push (cl-jobpond:scheduler-resolve scheduler :id (getf wakeup :id)
                                                         :token (getf wakeup :token) :action ':unknown) results)))))))
    (nreverse results)))

(-> application-mission-schedules-wake (application &key (:now (option integer))) list)
(defun application-mission-schedules-wake (application &key now)
  "Materialize due times and enqueue only proven current pending mission work."
  (let ((service (application-mission-schedules application)))
    (prog1
        (mission-schedule--present-outcomes
         application
         (with-recursive-lock-held ((mission-schedule-lock service))
           (mission-schedule--require-active service)
           (cl-jobpond:scheduler-wake (mission-schedule-scheduler service) :now now)
           (mission-schedule--admit service)))
      (mission-schedule--announce service))))

(-> application-mission-schedule-event (application string string &key (:payload t)) list)
(defun application-mission-schedule-event (application event id &key payload)
  "Deliver one stable external event and admit its current mission occurrences."
  (let ((service (application-mission-schedules application)))
    (prog1
        (mission-schedule--present-outcomes
         application
         (with-recursive-lock-held ((mission-schedule-lock service))
           (mission-schedule--require-active service)
           (cl-jobpond:scheduler-event (mission-schedule-scheduler service) :event event :id id :payload payload)
           (mission-schedule--admit service)))
      (mission-schedule--announce service)
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

(-> application-mission-schedules-awaited-p (application) boolean)
(defun application-mission-schedules-awaited-p (application)
  "Return true while a hosted time wakeup will resume APPLICATION's active mission.

Goal continuations defer to the next due or admitted occurrence of the exact
mission identity and version instead of re-prompting between occurrences.
Event schedules have no deadline and an unhosted scheduler never wakes, so
neither is awaited."
  (let ((goal    (application-goal application))
        (service (with-lock-held (*mission-schedule-services-lock*)
                   (gethash application *mission-schedule-services*))))
    (flet ((current-p (record)
             (let ((payload (getf record :payload)))
               (and (not (getf record :cancelled))
                    (equal (getf payload :mission) (getf goal :schedule-identity))
                    (equal (getf payload :version) (getf goal :schedule-version))))))
      (and (mission-goal-p goal)
           (eq (getf goal :status) ':active)
           (stringp (getf goal :schedule-identity))
           service
           (mission-schedule-notify service)
           (not (mission-schedule-retired-p service))
           (equal (mission-schedule-conversation service)
                  (conversation-identifier (application-conversation application)))
           (let ((snapshot (cl-jobpond:scheduler-snapshot (mission-schedule-scheduler service))))
             (or (some (lambda (schedule)
                         (and (current-p schedule) (getf schedule :next)))
                       (getf snapshot :schedules))
                 (some (lambda (wakeup)
                         (and (current-p wakeup) (member (getf wakeup :state) '(:pending :claimed))))
                       (getf snapshot :wakeups))))
           t))))

(-> application-mission-schedule-execute (application list) t)
(defun application-mission-schedule-execute (application ticket)
  "Execute an admitted ticket only once and recheck mission authority at consumption."
  (let* ((service   (application-mission-schedules application))
         (scheduler (mission-schedule-scheduler service))
         (wakeup    nil)
         (withheld  nil))
    (with-recursive-lock-held ((mission-schedule-lock service))
      (mission-schedule--require-active service)
      (setf wakeup (find (getf ticket :id) (getf (cl-jobpond:scheduler-snapshot scheduler) :wakeups)
                         :key (lambda (entry) (getf entry :id)) :test #'equal))
      (unless (and wakeup (eq (getf wakeup :state) ':claimed)
                   (eql (getf ticket :token) (getf wakeup :token)))
        (mission--reject ':schedule-ticket "Wakeup ticket is stale or requires explicit recovery."))
      (let ((disposition (if (getf wakeup :cancelled) ':stale
                            (mission-schedule--disposition service (getf wakeup :payload)))))
        (if (eq disposition ':runnable)
            (progn
              ;; Durable UNKNOWN prevents implicit replay after a crash or a copied command.
              (cl-jobpond:scheduler-resolve scheduler :id (getf wakeup :id)
                                                    :token (getf wakeup :token) :action ':unknown)
              (setf (gethash (getf wakeup :id) (mission-schedule-executing service)) t))
            (setf withheld (mission-schedule--withhold service wakeup disposition)))))
    (when withheld
      (mission-schedule--present-outcomes application (list withheld))
      (return-from application-mission-schedule-execute withheld))
    (multiple-value-prog1
        (unwind-protect
             (handler-case
                 (let* ((value   (funcall (mission-schedule-run service) application (getf (getf wakeup :payload) :content)))
                        (outcome (mission-schedule--runner-outcome value)))
                   (with-recursive-lock-held ((mission-schedule-lock service))
                     (case (mission-schedule-outcome-state outcome)
                       (:completed
                        (mission-schedule--settle service wakeup :outcome ':completed :result ':executed))
                       (:failed
                        (mission-schedule--settle
                         service wakeup :outcome ':failed
                         :result                 (if (eq (mission-schedule-outcome-reason outcome) ':provider-failed)
                                                     ':provider-failed ':runner-failed)))
                       (:unknown
                        wakeup))))
               (serious-condition (condition)
                 (let ((cause (if (and (typep condition 'mission-error) (mission-error-cause condition))
                                  (mission-error-cause condition) condition)))
                   (when (application-lisp--control-condition-p cause)
                     (error cause)))
                 ;; HANDLER-CASE runs only after terminal unwind, not at a recoverable signal.
                 (when (mission-schedule--provider-failure-p condition)
                   (with-recursive-lock-held ((mission-schedule-lock service))
                     (mission-schedule--settle service wakeup :outcome ':failed :result ':provider-failed)))
                 (error condition)))
          (with-recursive-lock-held ((mission-schedule-lock service))
            (remhash (getf wakeup :id) (mission-schedule-executing service))))
      ;; Publish only after normal return. Every nonlocal exit retains its durable
      ;; FAILED/UNKNOWN outcome for recovery without risking a second exit here.
      (mission-schedule--announce service))))

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
            (otherwise
             (mission--reject ':schedule-resolution "Expected completed, failed or retry."))))
      (mission-schedule--notify service))))

(-> application-mission-wakeup-slash-arguments (string) list)
(defun application-mission-wakeup-slash-arguments (remainder)
  "Read the serialized ticket only at the slash command boundary."
  (list (mission--read-data remainder)))

(-> application-mission-wakeup-command (application t &rest t) null)
(defun application-mission-wakeup-command (application ticket &rest extra)
  "Validate and consume a native persisted scheduler ticket."
  (unless (and (null extra) (proper-list-p ticket) (= (length ticket) 4)
               (every (lambda (key) (member key '(:id :token)))
                      (plist-keys ticket))
               (proper-list-p (getf ticket :id)) (getf ticket :id)
               (integerp (getf ticket :token)) (plusp (getf ticket :token)))
    (mission--reject ':schedule-ticket "Expected a ticket with :id and a positive integer :token."))
  (unless (eq application
              (mission-schedule--application (application-operation--tool-context application)))
    (mission--reject ':schedule-authority "Only the owning primary application may consume a wakeup."))
  (application-mission-schedule-execute application ticket)
  nil)

(define-application-command application--builtin-mission-wakeup-command
    (:name              "/mission-wakeup"
     :argument          "TICKET"
     :description       "consume an admitted durable mission wakeup"
     :tip               "rechecks the current mission version and records uncertain execution before effects."
     :busy-behavior     :hold
     :terminal-behavior :shared
     :slash-parser      application-mission-wakeup-slash-arguments
     :callable          t)
    (application &optional ticket &rest extra)
  (apply #'application-mission-wakeup-command application ticket extra)
  ':continue)

(-> mission-schedule--runner-outcome (t) mission-schedule-outcome)
(defun mission-schedule--runner-outcome (value)
  "Accept typed outcomes; retain only legacy custom runners' NIL/list returns.
Explicit failure/unknown keywords are never treated as successful execution."
  (cond ((typep value 'mission-schedule-outcome)
         value)
        ((eq value ':failed)
         (make-mission-schedule-outcome :state ':failed :reason ':runner-failed))
        ((eq value ':unknown)
         (make-mission-schedule-outcome))
        ;; Older injected runners return their accumulated execution list or NIL.
        ((listp value)
         (make-mission-schedule-outcome :state ':completed :reason ':executed))
        (t
         (make-mission-schedule-outcome))))

(-> mission-schedule--provider-failure-p (serious-condition) boolean)
(defun mission-schedule--provider-failure-p (condition)
  "Classify terminal provider provenance, never a generic mission failure or report."
  (or (typep condition 'provider-error)
      (and (typep condition 'mission-error)
           (eq (mission-error-reason condition) ':failed)
           (typep (mission-error-cause condition) 'provider-error))))

(-> mission-schedule--append-notice (mission-schedule-service string list) list)
(defun mission-schedule--append-notice (service source spans)
  "Append to the captured owner and return the durable record without presenting it."
  (let* ((conversation (mission-schedule-notice-conversation service))
         (properties   (list :source source :spans spans)))
    (application-side-status--validate-record
     conversation (list* :seq  (conversation-next-sequence conversation)
                         :time (get-universal-time) properties))
    (conversation-append-record conversation (list* :side-status properties))))

(-> mission-schedule--notice-current-p (mission-schedule-service list) boolean)
(defun mission-schedule--notice-current-p (service wakeup)
  "Recheck a notice snapshot while holding SERVICE's reservation lock."
  (let ((current (find (getf wakeup :id)
                       (getf (cl-jobpond:scheduler-snapshot (mission-schedule-scheduler service)) :wakeups)
                       :key (lambda (entry) (getf entry :id)) :test #'equal)))
    (and (not (mission-schedule-retired-p service))
         (not (gethash (getf wakeup :id) (mission-schedule-executing service)))
         (not (null (member (getf current :state) '(:failed :unknown))))
         (eql (getf current :token) (getf wakeup :token))
         (eq (getf current :state) (getf wakeup :state))
         (equal (getf current :result) (getf wakeup :result)))))

(-> mission-schedule--publish-notice (mission-schedule-service list keyword) null)
(defun mission-schedule--publish-notice (service wakeup reason)
  "Revalidate and append atomically with recovery; present outside the scheduler lock."
  (let* ((application  (mission-schedule-application service))
         (conversation (mission-schedule-notice-conversation service))
         (source       (format nil "mission-schedule occurrence ~A claim ~D ~A"
                               (getf wakeup :id) (getf wakeup :token) (getf wakeup :state)))
         (body         (ecase reason
                         (:provider-failed
                          "Provider turn failed; not retried. Effects may be partial.")
                         (:runner-failed
                          "Scheduled work failed; not retried. Effects may be partial.")
                         (:admission-refused
                          "Scheduled work admission refused; not retried.")
                         (:interrupted
                          "Scheduled work interrupted; outcome unknown. Effects may be partial.")))
         (spans        (if (application-ui application)
                           (application--transcript-entry application :style ':failure
                                                         :header "Scheduled occurrence" :body body)
                           (list (terminal-span ':failure "Scheduled occurrence")
                                 (terminal-span ':plain (format nil "~%~A" body)))))
         (record       nil))
    (with-recursive-lock-held (*mission-schedule-notice-lock*)
      (with-recursive-lock-held ((mission-schedule-lock service))
        (when (mission-schedule--notice-current-p service wakeup)
          (with-recursive-lock-held ((conversation-append-lock conversation))
            (let ((found-p nil))
              (conversation-map-records
               conversation (lambda (entry)
                              (when (and (eq (first entry) ':side-status)
                                         (equal source (getf (rest entry) :source)))
                                (setf found-p t))))
              (unless found-p
                (setf record (mission-schedule--append-notice service source spans)))))))
      ;; Conversation replacement uses the notice lock, not the scheduler lock.
      (when (and record (eq conversation (application-conversation application))
                 (application-ui application))
        (application--present-conversation-record application record))))
  nil)

(-> mission-schedule--announce (mission-schedule-service) null)
(defun mission-schedule--announce (service)
  "Reconcile retained FAILED/UNKNOWN occurrences with durable notice history.
Jobpond retains terminal wakeups and results without eviction. Publication rechecks
snapshots under the reservation lock; terminal presentation runs outside that lock."
  (let ((pending nil))
    (with-recursive-lock-held ((mission-schedule-lock service))
      (unless (mission-schedule-retired-p service)
        (dolist (wakeup (getf (cl-jobpond:scheduler-snapshot (mission-schedule-scheduler service)) :wakeups))
          (unless (gethash (getf wakeup :id) (mission-schedule-executing service))
            (case (getf wakeup :state)
              (:unknown
               (push (cons wakeup ':interrupted) pending))
              (:failed
               (push (cons wakeup (case (getf wakeup :result)
                                   (:provider-failed
                                    ':provider-failed)
                                   (:admission-refused
                                    ':admission-refused)
                                   (otherwise
                                    ':runner-failed))) pending)))))))
    (dolist (entry (nreverse pending))
      (mission-schedule--publish-notice service (first entry) (rest entry))))
  nil)
