;;;; -- Model Collector Replay Hooks --

(in-package #:autolith-supervisor)

(declaim (ftype (function (hash-table hash-table) null) model-replay--verify-event))

(defun model-replay--verify-event (expected actual)
  "Require exact event identity without writing a second journal entry."
  (unless (model-replay-store--exact-p (model-replay-store--portable expected)
                                     (model-replay-store--portable actual))
    (error 'model-replay-store-error :code ':event-mismatch))
  nil)

(declaim (ftype (function (&key (:replay-store model-replay-store) (:session-store session-store)
                                (:request-id string) (:turn-id string) (:profile model-profile)
                                (:emit-event function) (:hosted-types hash-table) (:kind keyword) (:purpose keyword)
                                (:require-item-delta-identity boolean)) model-event-collector)
                make-model-replay-collector))

(defun make-model-replay-collector (&key replay-store session-store request-id turn-id profile emit-event
                                        (hosted-types (make-hash-table :test 'equal))
                                        (kind ':generate) (purpose ':ordinary) require-item-delta-identity)
  "Connect one collector to the real replay and session journals.

The caller owns admission, provider execution, cancellation, and transport limits.
Call from one owner for a new request. Persist each public event once, then verify
its indexed identity before emission. Accept and publish the session note before
acknowledging the collector. On uncertain acceptance, reconcile the replay outbox;
never turn a possibly committed result into a terminal failure."
  (unless (and (string= (model-replay-store-session replay-store) (session-store-id session-store))
               (session-store--identifier-p request-id) (session-store--identifier-p turn-id))
    (error 'model-replay-store-error :code ':invalid-request))
  (let ((request       (copy-seq request-id))
        (turn          (copy-seq turn-id))
        (profile-id    (copy-seq (model-profile-id profile)))
        (family        (string-downcase (model-profile-family profile)))
        (attempt       0)
        (pending-event nil))
    (make-model-event-collector
     :profile profile :hosted-types hosted-types :kind kind
     :require-item-delta-identity require-item-delta-identity
     :emit-event                  emit-event
     :record-item
     (lambda (&key ((:attempt item-attempt)) item-index original view)
       (unless (and (= item-attempt attempt) (null pending-event)
                    (= item-index (parent-proto:output-item.item-index view))
                    (string= family (parent-proto:output-item.family view)))
         (error 'model-replay-store-error :code ':event-mismatch))
       (let ((data (protocol-message->data view)))
         (dolist (key '("item_index" "replay_handle" "family")) (remhash key data))
         (multiple-value-bind (handle event)
             (model-replay-store-put-item replay-store :request-id request :attempt attempt
                                                      :original original :view data)
           (setf pending-event event)
           handle)))
     :retain-state
     (lambda (&key kind value)
       (model-replay-store-retain-state replay-store :request-id request :attempt attempt
                                                    :kind kind :value value))
     :accept-result
     (lambda (complete)
       (model-replay-store-accept replay-store :request-id request :attempt attempt
                                              :complete (protocol-message->data complete)
                                              :request-kind kind :request-purpose purpose)
       (model-replay-store-project-session replay-store session-store)
       (let ((events (model-replay-store-events replay-store request)))
         (setf pending-event (aref events (1- (length events)))))
       t)
     :record-event
     (lambda (event)
       (let* ((data          (protocol-message->data event))
              (event-attempt (parent-proto:model-event.attempt event))
              (stored
                (cond
                  ((parent-proto:model-event.has-attempt-started event)
                   (when pending-event
                     (error 'model-replay-store-error :code ':event-mismatch))
                   (setf attempt event-attempt)
                   (let ((start (gethash "attempt_started" data)))
                     (model-replay-store-start-attempt
                      replay-store :request-id request :turn-id turn :profile-id profile-id :family family
                      :attempt attempt :maximum-attempts (gethash "maximum_attempts" start)
                      :delay-milliseconds (gethash "delay_milliseconds" start))))
                  ((or (parent-proto:model-event.has-item-completed event)
                       (parent-proto:model-event.has-complete event))
                   (unless pending-event
                     (error 'model-replay-store-error :code ':event-mismatch))
                   pending-event)
                  ((parent-proto:model-event.has-failed event)
                   (model-replay-store-fail-request replay-store :request-id request :attempt attempt
                                                               :failed (gethash "failed" data)))
                  (t
                   (let ((keys (loop for key being the hash-keys of data
                                     unless (member key '("index" "attempt") :test #'equal)
                                       collect key)))
                     (unless (and (= event-attempt attempt) (= 1 (length keys)) (null pending-event))
                       (error 'model-replay-store-error :code ':event-mismatch))
                     (prog1 (model-replay-store-append-event
                             replay-store :request-id request :attempt attempt
                             :kind (first keys) :payload (gethash (first keys) data))
                       (when (parent-proto:model-event.has-attempt-failed event)
                         (model-replay-store-fail-attempt replay-store :request-id request :attempt attempt))))))))
         (model-replay--verify-event stored data)
         (setf pending-event nil))))))

(declaim (ftype (function (model-replay-store &key (:turn-id string)) function)
                make-model-replay-resolver))

(defun make-model-replay-resolver (replay-store &key turn-id)
  "Return a request-scoped resolver for MODEL-START->WIRE-REQUEST.
The trusted service must admit TURN-ID before creating this resolver. The store
checks session, profile, family, kind, turn-state owner, and accepted publication."
  (unless (session-store--identifier-p turn-id)
    (error 'model-replay-store-error :code ':invalid-request))
  (let ((turn (copy-seq turn-id)))
    (lambda (handle &key kind profile turn-id)
      (unless (and (stringp turn-id) (string= turn turn-id) (typep profile 'model-profile))
        (error 'model-replay-store-error :code ':invalid-handle))
      (let ((scope (list :session-id (model-replay-store-session replay-store)
                         :profile-id (model-profile-id profile)
                         :family     (string-downcase (model-profile-family profile)))))
        (values
         (case kind
           (:item
            (apply #'model-replay-store-resolve replay-store handle scope))
           ((:continuation :turn-state)
            (apply #'model-replay-store-resolve-state replay-store handle :kind kind :turn-id turn scope))
           (otherwise
            (error 'model-replay-store-error :code ':invalid-handle)))
         t)))))
