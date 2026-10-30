;;;; -- Active Parent Input in the Isolated Child --

(in-package #:autolith)

(-> agent-application-input-start (agent-protocol-session) null)
(defun agent-application-input-start (session)
  "Start the FIFO application worker without terminal or private settings access."
  (setf (agent-protocol-session-application-worker session)
        (bordeaux-threads:make-thread
         (lambda () (agent-application-input--work session)) :name "parent application FIFO"))
  nil)

(-> agent-application-input-deliver (agent-protocol-session agent-wire:envelope) boolean)
(defun agent-application-input-deliver (session envelope)
  "Admit one parent-created scope while the caller holds the session lock."
  (let* ((steering   (agent-wire:envelope.application-steering envelope))
         (identifier (agent-wire:application-steering.operation-id steering)))
    (if (or (agent-protocol-session-input-closed session)
            (gethash identifier (agent-protocol-session-application-identifiers session))
            (>= (hash-table-count (agent-protocol-session-application-identifiers session)) 64))
        nil
        (progn
          (setf (gethash identifier (agent-protocol-session-application-identifiers session)) ':pending
                (agent-protocol-session-application-input session)
                (nconc (agent-protocol-session-application-input session) (list steering)))
          (bordeaux-threads:condition-notify (agent-protocol-session-application-ready session))
          t))))

(-> agent-application-input-take-steering (agent-protocol-session) list)
(defun agent-application-input-take-steering (session)
  "Return accepted message input to the active provider at its next safe boundary."
  (bordeaux-threads:with-lock-held ((agent-protocol-session-lock session))
    (prog1 (agent-protocol-session-steering session)
      (setf (agent-protocol-session-steering session) nil))))

(-> agent-application-input-steering-persisted (agent-protocol-session string) null)
(defun agent-application-input-steering-persisted (session identifier)
  "Wake the FIFO only after the active provider committed the steering message."
  (bordeaux-threads:with-lock-held ((agent-protocol-session-lock session))
    (setf (gethash identifier (agent-protocol-session-application-identifiers session)) ':persisted)
    (bordeaux-threads:condition-notify (agent-protocol-session-application-ready session))
    (bordeaux-threads:condition-notify (agent-protocol-session-ready session)))
  nil)

(-> agent-application-input--message (agent-protocol-session agent-wire:application-steering)
    agent-wire:application-result)
(defun agent-application-input--message (session steering)
  "Wait for a message's exact FIFO position to reach a provider safe boundary."
  (let* ((action     (agent-wire:application-steering.action steering))
         (identifier (agent-wire:application-steering.operation-id steering))
         (message    (user-message-input-create
                      :text            (agent-wire:application-action.source action)
                      :image-pathnames (mapcar #'pathname (agent-wire:application-action.image-paths action)))))
    (bordeaux-threads:with-lock-held ((agent-protocol-session-lock session))
      (setf (agent-protocol-session-steering session)
            (nconc (agent-protocol-session-steering session)
                   (list (agent-steering-input-create :identifier identifier :content message))))
      (bordeaux-threads:condition-notify (agent-protocol-session-ready session))
      (loop until (or (eq (gethash identifier (agent-protocol-session-application-identifiers session)) ':persisted)
                      (agent-protocol-session-cancelled session)
                      (agent-protocol-session-stop session)
                      (agent-protocol-session-failure session))
            do (bordeaux-threads:condition-wait
                (agent-protocol-session-application-ready session) (agent-protocol-session-lock session))))
    (when (agent-protocol-session-cancelled-p session)
      (bordeaux-threads:with-lock-held ((agent-protocol-session-lock session))
        (setf (agent-protocol-session-steering session)
          (remove identifier (agent-protocol-session-steering session)
            :key #'agent-steering-input-identifier :test #'equal))))
    (agent-wire:make-application-result
     :turn-id identifier :status (if (agent-protocol-session-cancelled-p session) ':aborted ':ok))))

(-> agent-application-input--work (agent-protocol-session) null)
(defun agent-application-input--work (session)
  "Run non-message actions in child heap state, outside the provider's blocking request."
  (handler-case
      (loop
        (let ((steering nil))
          (bordeaux-threads:with-lock-held ((agent-protocol-session-lock session))
            (loop until (or (agent-protocol-session-application-input session)
                            (agent-protocol-session-stop session)
                            (agent-protocol-session-failure session))
                  do (bordeaux-threads:condition-wait
                      (agent-protocol-session-application-ready session) (agent-protocol-session-lock session)))
            (when (agent-protocol-session-failure session)
              (return))
            (setf steering (pop (agent-protocol-session-application-input session)))
            (unless steering
              (return))
            (setf (agent-protocol-session-application-active session) t))
          (unwind-protect
               (let* ((identifier                   (agent-wire:application-steering.operation-id steering))
                      (action                       (agent-wire:application-steering.action steering))
                      (*agent-application-scope-id* identifier)
                      (result
                        (handler-case
                            (if (eq (agent-wire:application-action.kind action) ':message)
                                (agent-application-input--message session steering)
                                (agent-protocol-application-run session action))
                          (application-turn-cancelled ()
                            (agent-wire:make-application-result :turn-id identifier :status ':aborted))
                          (error (condition)
                            (agent-wire:make-application-result
                             :turn-id identifier :status ':error :condition (princ-to-string condition))))))
                 (setf (agent-wire:application-result.turn-id result) identifier)
                 (agent-application-result-publish session result)
                 (bordeaux-threads:with-lock-held ((agent-protocol-session-lock session))
                   (setf (gethash identifier (agent-protocol-session-application-identifiers session)) ':finished)))
            (bordeaux-threads:with-lock-held ((agent-protocol-session-lock session))
              (setf (agent-protocol-session-application-active session) nil)
              (bordeaux-threads:condition-notify (agent-protocol-session-ready session))))))
    (serious-condition (condition)
      (agent-session--disconnect session condition)
      (agent-protocol-driver-close (agent-protocol-session-driver session) :condition condition)))
  nil)

(-> agent-application-input-finish (agent-protocol-session agent-observer) null)
(defun agent-application-input-finish (session observer)
  "Drain every admitted scope before TurnFinish; refuse late input explicitly."
  (loop
    (let ((steering-p nil) (done nil))
      (bordeaux-threads:with-lock-held ((agent-protocol-session-lock session))
        (when (agent-protocol-session-failure session)
          (return))
        (setf steering-p (not (null (agent-protocol-session-steering session))))
        (when (and (null (agent-protocol-session-application-input session))
                   (not (agent-protocol-session-application-active session)))
          (setf done t
                (agent-protocol-session-input-closed session) t))
        (unless (or done steering-p)
          (bordeaux-threads:condition-wait (agent-protocol-session-ready session)
                                          (agent-protocol-session-lock session) :timeout 0.01)))
      (when done
        (return))
      (when (and steering-p (not (agent-protocol-session-cancelled-p session)))
        (agent-session--run-default session observer))))
  nil)
