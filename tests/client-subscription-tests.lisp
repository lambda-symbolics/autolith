(in-package #:autolith)

;;;; -- Shared Subscription Boundaries --

(defclass client-subscription-test-client (channel-subscription)
  ((call-function :initform nil :accessor client-subscription-test-call-function
                  :documentation "Optional controlled request implementation.")
   (present-function :initform nil :accessor client-subscription-test-present-function
                     :documentation "Optional synchronous presentation barrier.")
   (events :initform nil :accessor client-subscription-test-events
           :documentation "Events presented in reverse delivery order."))
  (:documentation "A frontend with controllable request and presentation boundaries."))

(defmethod channel-subscription-call ((client client-subscription-test-client) command &rest arguments)
  "Use a controlled request when installed, otherwise exercise the real transport."
  (let ((function (client-subscription-test-call-function client)))
    (if function
        (funcall function command arguments)
        (call-next-method))))

(defmethod channel-subscription-present-event ((client client-subscription-test-client) event)
  "Complete presentation before recording the delivered event."
  (let ((function (client-subscription-test-present-function client)))
    (when function (funcall function event)))
  (push event (client-subscription-test-events client))
  nil)

(-> client-subscription-tests--start (function) (values t t cons))
(defun client-subscription-tests--start (function)
  "Start FUNCTION and return its thread, completion semaphore and tagged outcome cell."
  (let* ((done    (sb-thread:make-semaphore))
         (outcome (list nil nil))
         (thread
           (make-thread
            (lambda ()
              (unwind-protect
                   (handler-case (setf (first outcome) ':ok
                                       (second outcome) (funcall function))
                     (error (condition)
                       (setf (first outcome) ':error
                             (second outcome) condition)))
                (sb-thread:signal-semaphore done)))
            :name "Subscription boundary caller")))
    (values thread done outcome)))

(-> client-subscription-tests--finished (list integer &key (:client (option string)) (:id (option string))) list)
(defun client-subscription-tests--finished (submission sequence &key client id)
  "Build a completion with an optional mismatched client or submission identifier."
  (list :work-finished :sequence sequence :outcome ':done :idle-p nil
        :submission (list :id     (or id (getf submission :id))
                          :client (or client (getf submission :client)))))

(-> test-client-subscription-exact-completion () null)
(defun test-client-subscription-exact-completion ()
  "Neither channel idleness nor another client's completion settles the caller's request."
  (let ((client     (make-instance 'client-subscription-test-client :identifier "exact"))
        (submitted  (sb-thread:make-semaphore))
        (submission nil))
    (setf (client-subscription-test-call-function client)
          (lambda (command arguments)
            (declare (ignore command))
            (setf submission (getf arguments :submission))
            (sb-thread:signal-semaphore submitted)
            '(:accepted t)))
    (multiple-value-bind (thread done outcome)
        (client-subscription-tests--start
         (lambda () (channel-subscription-await-submission client "request" :source "script")))
      (unwind-protect
           (progn
             (test-assert (sb-thread:wait-on-semaphore submitted :timeout 3) "the request is submitted")
             (test-assert (and (equal "script" (getf submission :source))
                               (equal (channel-subscription-client-identifier client)
                                      (getf submission :client)))
                          "submission metadata carries the frontend identity and source")
             (channel-subscription-event client '(:idle :sequence 1))
             (channel-subscription-event
              client (client-subscription-tests--finished submission 2 :client "other-client"))
             (channel-subscription-event
              client (client-subscription-tests--finished submission 3 :id "other-request"))
             (test-assert (not (sb-thread:wait-on-semaphore done :timeout 0.05))
                          "idle and mismatched completions do not release the request")
             (channel-subscription-event client (client-subscription-tests--finished submission 4))
             (test-assert (sb-thread:wait-on-semaphore done :timeout 3)
                          "its exact completion releases the request while the channel is not idle")
             (test-assert (and (eq (first outcome) ':ok)
                               (eq (getf (second outcome) :outcome) ':done)
                               (not (getf (second outcome) :idle-p))
                               (not (channel-subscription-submission-active-p client)))
                          "the result belongs to this completed request and releases client admission"))
        (channel-subscription-close client)
        (join-thread thread))))
  nil)

(-> test-client-subscription-early-completion () null)
(defun test-client-subscription-early-completion ()
  "Retain a terminal callback delivered before the submit reply returns."
  (let ((client    (make-instance 'client-subscription-test-client :identifier "early"))
        (presented (sb-thread:make-semaphore))
        (reply     (sb-thread:make-semaphore)))
    (setf (client-subscription-test-call-function client)
          (lambda (command arguments)
            (declare (ignore command))
            (channel-subscription-event
             client (client-subscription-tests--finished (getf arguments :submission) 1))
            (sb-thread:signal-semaphore presented)
            (sb-thread:wait-on-semaphore reply)
            '(:accepted t)))
    (multiple-value-bind (thread done outcome)
        (client-subscription-tests--start
         (lambda () (channel-subscription-await-submission client "early")))
      (unwind-protect
           (progn
             (test-assert (sb-thread:wait-on-semaphore presented :timeout 3)
                          "the terminal callback completes before the submit reply")
             (test-assert (and (channel-subscription-submission-result client)
                               (not (sb-thread:wait-on-semaphore done :timeout 0.05)))
                          "the early result is retained while the request still owns its transport call")
             (sb-thread:signal-semaphore reply)
             (test-assert (sb-thread:wait-on-semaphore done :timeout 3)
                          "returning the submit reply consumes the already delivered completion")
             (test-assert (eq (first outcome) ':ok) "the early completion is not lost"))
        (sb-thread:signal-semaphore reply)
        (channel-subscription-close client)
        (join-thread thread))))
  nil)

(-> test-client-subscription-presentation-boundary () null)
(defun test-client-subscription-presentation-boundary ()
  "Do not advance a delivery cursor or settle a waiter before synchronous presentation completes."
  (let ((client     (make-instance 'client-subscription-test-client :identifier "presentation"))
        (submitted  (sb-thread:make-semaphore))
        (entered    (sb-thread:make-semaphore))
        (release    (sb-thread:make-semaphore))
        (submission nil)
        (delivery   nil))
    (setf (client-subscription-test-call-function client)
          (lambda (command arguments)
            (declare (ignore command))
            (setf submission (getf arguments :submission))
            (sb-thread:signal-semaphore submitted)
            '(:accepted t))
          (client-subscription-test-present-function client)
          (lambda (event)
            (when (eq (first event) ':work-finished)
              (sb-thread:signal-semaphore entered)
              (sb-thread:wait-on-semaphore release))))
    (multiple-value-bind (thread done outcome)
        (client-subscription-tests--start
         (lambda () (channel-subscription-await-submission client "present")))
      (unwind-protect
           (progn
             (test-assert (sb-thread:wait-on-semaphore submitted :timeout 3) "the request is pending")
             (setf delivery
                   (make-thread
                    (lambda ()
                      (channel-subscription-event
                       client (client-subscription-tests--finished submission 1)))
                    :name "Blocked subscription presenter"))
             (test-assert (sb-thread:wait-on-semaphore entered :timeout 3)
                          "the terminal notification reaches its presentation boundary")
             (test-assert (and (zerop (channel-subscription-event-sequence client))
                               (null (channel-subscription-submission-result client))
                               (not (sb-thread:wait-on-semaphore done :timeout 0.05)))
                          "neither replay cursor nor waiter acknowledges unfinished notification delivery")
             (sb-thread:signal-semaphore release)
             (test-assert (sb-thread:wait-on-semaphore done :timeout 3)
                          "completed presentation releases the waiter")
             (test-assert (and (eq (first outcome) ':ok)
                               (= 1 (channel-subscription-event-sequence client))
                               (= 1 (length (client-subscription-test-events client))))
                          "presentation, cursor advancement and request completion commit together"))
        (sb-thread:signal-semaphore release)
        (channel-subscription-close client)
        (when delivery (join-thread delivery))
        (join-thread thread))))
  nil)

(-> test-client-subscription-attachment-cursor () null)
(defun test-client-subscription-attachment-cursor ()
  "Buffer attachment callbacks and deliver only events newer than the installed cursor once."
  (let ((client (make-instance 'client-subscription-test-client :identifier "cursor")))
    (unwind-protect
         (progn
           (test-assert (zerop (channel-subscription-begin-attachment client))
                        "attachment begins at the last fully presented cursor")
           (dolist (sequence '(2 3 3 4))
             (channel-subscription-event client (list :present :sequence sequence)))
           (test-assert (null (client-subscription-test-events client))
                        "attachment callbacks wait for history installation")
           (channel-subscription-finish-attachment client '(:attached :sequence 2 :record-sequence 10)
                                                   :replay-p t)
           (channel-subscription-event client '(:present :sequence 4))
           (channel-subscription-event client '(:present :sequence 5))
           (test-assert (equal '(3 4 5)
                               (mapcar (lambda (event) (getf (rest event) :sequence))
                                       (reverse (client-subscription-test-events client))))
                        "snapshot-covered events and duplicate callbacks are omitted without reordering")
           (test-assert (and (= 5 (channel-subscription-event-sequence client))
                             (not (channel-subscription-attaching-p client)))
                        "drained attachment reopens admission at the final delivered cursor"))
      (channel-subscription-close client)))
  nil)

(-> test-client-subscription-closure-wakes-waiter () null)
(defun test-client-subscription-closure-wakes-waiter ()
  "Both explicit close and an unexpected transport closure wake exact-completion waiters."
  (dolist (close-kind '(:explicit :transport))
    (let ((client    (make-instance 'client-subscription-test-client :identifier "closure"))
          (submitted (sb-thread:make-semaphore)))
      (setf (client-subscription-test-call-function client)
            (lambda (command arguments)
              (declare (ignore command arguments))
              (sb-thread:signal-semaphore submitted)
              '(:accepted t)))
      (multiple-value-bind (thread done outcome)
          (client-subscription-tests--start
           (lambda () (channel-subscription-await-submission client "pending")))
        (unwind-protect
             (progn
               (test-assert (sb-thread:wait-on-semaphore submitted :timeout 3) "a submission awaits completion")
               (ecase close-kind
                 (:explicit
                  (channel-subscription-close client))
                 (:transport
                  (channel-subscription-event client '(:closed))))
               (test-assert (sb-thread:wait-on-semaphore done :timeout 3)
                            "closure wakes the waiter without any completion event")
               (test-assert (and (eq (first outcome) ':error)
                                 (typep (second outcome) 'channel-subscription-unavailable)
                                 (not (channel-subscription-submission-active-p client)))
                            "a closed request reports unavailability and releases its active slot"))
          (channel-subscription-close client)
          (join-thread thread)))))
  nil)

(-> test-client-subscription-reconnect-drain-admission () null)
(defun test-client-subscription-reconnect-drain-admission ()
  "A real transport request cannot overtake buffered presentation after replacement installation."
  (client-protocol-tests--call
   (lambda (application entry)
     (declare (ignore application))
     (let* ((client      (make-instance 'client-subscription-test-client :identifier "admission"))
            (previous    (localgroup-client-connect entry :approves-p nil))
            (replacement (localgroup-client-connect entry :approves-p nil))
            (entered     (sb-thread:make-semaphore))
            (release     (sb-thread:make-semaphore))
            (started     (sb-thread:make-semaphore))
            (reconnect   nil))
       (setf (channel-subscription-connection client) previous
             (client-subscription-test-present-function client)
             (lambda (event)
               (declare (ignore event))
               (sb-thread:signal-semaphore entered)
               (sb-thread:wait-on-semaphore release)))
       (unwind-protect
           (test-call-with-function-replacements
            (list
             (list 'channel-subscription--connect-replacement
                   (lambda (&rest arguments)
                     (declare (ignore arguments))
                     (channel-subscription-event client '(:present :sequence 1))
                     replacement)))
            (lambda ()
              (channel-subscription-event client '(:checkpoint-reconnect :id "replacement"))
              (with-lock-held ((channel-subscription-lock client))
                (setf (channel-subscription-reconnect-thread client)
                      (setf reconnect
                              (make-thread
                               (lambda ()
                                 (channel-subscription--reconnect client previous :identifier
                                  "replacement" :sequence 0))
                               :name "Reconnect attachment drain"))))
              (test-assert (sb-thread:wait-on-semaphore entered :timeout 3)
               "the replacement is installed while buffered presentation is unfinished")
              (test-assert
               (and (eq replacement (channel-subscription-connection client))
                    (null (channel-subscription-reconnect-id client))
                    (and (eq reconnect (channel-subscription-reconnect-thread client))
                         (channel-subscription-attaching-p client)))
               "replacement installation does not reopen admission before drain")
              (multiple-value-bind (thread done outcome)
                  (client-subscription-tests--start
                   (lambda ()
                     (sb-thread:signal-semaphore started)
                     (channel-subscription-call client ':state)))
                (unwind-protect
                    (progn
                     (test-assert (sb-thread:wait-on-semaphore started :timeout 3)
                      "the new request begins")
                     (test-assert (not (sb-thread:wait-on-semaphore done :timeout 0.05))
                      "clearing reconnect identity does not bypass the attachment barrier")
                     (sb-thread:signal-semaphore release)
                     (test-assert (sb-thread:wait-on-semaphore done :timeout 3)
                      "the state request reaches the server after attachment drain")
                     (test-assert
                      (and (eq (first outcome) ':ok)
                           (eq (first (getf (second outcome) :state)) ':localgroup-status))
                      "the admitted request preserves its real transport reply"))
                  (sb-thread:signal-semaphore release)
                  (channel-subscription-close client)
                  (join-thread thread)))))
         (sb-thread:signal-semaphore release)
         (channel-subscription-close client)
         (when reconnect (join-thread reconnect))
         (localgroup-client-close previous)
         (localgroup-client-close replacement)))))
  nil)

(-> test-client-subscription-replay-refusal-wakes-waiter () null)
(defun test-client-subscription-replay-refusal-wakes-waiter ()
  "A replay-cursor refusal ends reconnect and wakes a request still awaiting its own completion."
  (client-protocol-tests--call
   (lambda (application entry)
     (let ((client
            (make-instance 'client-subscription-test-client :identifier "replay-refusal"
                           :approves-p nil :configuration (application-configuration application)))
           (submitted (sb-thread:make-semaphore))
           (attempts  0))
       (setf (channel-subscription-connection client)
             (localgroup-client-connect entry :approves-p nil)
             (client-subscription-test-call-function client)
             (lambda (command arguments)
               (declare (ignore command arguments))
               (sb-thread:signal-semaphore submitted)
               '(:accepted t)))
       (multiple-value-bind (thread done outcome)
           (client-subscription-tests--start
            (lambda () (channel-subscription-await-submission client "pending-replay")))
         (unwind-protect
             (test-call-with-function-replacements
              (list
               (list 'localgroup--wait-for-checkpoint-entry
                     (lambda (&rest arguments) (declare (ignore arguments)) entry))
               (list 'localgroup-client-connect
                     (lambda (&rest arguments)
                       (progn
                        (test-assert (null (getf (rest arguments) :approves-p :missing))
                         "reconnect preserves a non-approving client's capability")
                        (incf attempts))
                       (error 'localgroup-client-rejected :message "Replay cursor expired."
                              :operation                           ':attach))))
              (lambda ()
                (test-assert (sb-thread:wait-on-semaphore submitted :timeout 3)
                 "the request is awaiting replayed completion")
                (channel-subscription-event client '(:checkpoint-reconnect :id "checkpoint"))
                (channel-subscription-event client '(:closed))
                (test-assert (sb-thread:wait-on-semaphore done :timeout 3)
                 "replay refusal wakes the unfinished request")
                (let ((worker (channel-subscription-reconnect-thread client)))
                  (when worker (join-thread worker)))
                (test-assert
                 (and (= 1 attempts) (eq (first outcome) ':error)
                      (typep (second outcome) 'channel-subscription-unavailable)
                      (channel-subscription-closed-p client)
                      (non-empty-string-p (channel-subscription-reconnect-failure client)))
                 "an explicit replay refusal is terminal, diagnostic and never retried")))
           (channel-subscription-close client)
           (join-thread thread))))))
  nil)
