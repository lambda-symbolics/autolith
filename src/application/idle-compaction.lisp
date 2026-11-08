(in-package #:autolith)

;;;; -- Idle Prompt-Cache Compaction --

(defparameter *idle-compaction-lead-seconds* 120
  "How long before estimated cache expiry to begin idle compaction.")

(defparameter *idle-compaction-minimum-tokens* 32768
  "The minimum completed prompt size worth an additional idle compaction request.")

(defparameter *idle-compaction-minimum-history-characters* 131072
  "The minimum summarizable text size, independent of instructions and tool schemas.")

(defparameter *idle-compaction-clock-function*
  (lambda () (/ (get-internal-real-time) internal-time-units-per-second))
  "The process-local monotonic clock for cache opportunities, in seconds.")

(-> application-idle-compaction-note-started
    (application &key (:now real)) null)
(defun application-idle-compaction-note-started
    (application &key (now (funcall *idle-compaction-clock-function*)))
  "Replace the cache opportunity at an ordinary provider request's start."
  (let ((agent (and (slot-boundp application 'agent) (application-agent application))))
    (setf (application-idle-compaction-episode application)
          (when agent
            (make-instance 'prompt-cache-episode
                           :conversation (application-conversation application)
                           :provider     (agent-provider agent)
                           :model        (copy-seq (config :model (application-configuration application)))
                           :started-at   now))))
  nil)

(-> application-idle-compaction--history-characters (t) (integer 0))
(defun application-idle-compaction--history-characters (value)
  "Count summarizable text in VALUE, stopping at the idle-compaction threshold.

Ignore image URLs, encoded private content, and provider metadata. This is a
size heuristic, not a token or pricing estimate."
  (let ((remaining *idle-compaction-minimum-history-characters*))
    (labels ((visit (value)
               (when (plusp remaining)
                 (typecase value
                   (string
                    (decf remaining (min remaining (length value))))
                   (hash-table
                    (dolist (key '("text" "content" "arguments" "output" "summary"))
                      (visit (gethash key value))))
                   (list
                    (loop for item in value while (plusp remaining) do (visit item)))
                   (vector
                    (loop for item across value while (plusp remaining) do (visit item)))))))
      (visit value))
    (- *idle-compaction-minimum-history-characters* remaining)))

(-> application-idle-compaction-note-completed (application t) null)
(defun application-idle-compaction-note-completed (application usage)
  "Seal the current ordinary request's opportunity with its completed USAGE."
  (let ((episode (application-idle-compaction-episode application)))
    (when (and episode
               (eq (prompt-cache-episode-conversation episode)
                   (application-conversation application)))
      (setf (prompt-cache-episode-prompt-tokens episode)
            (conversation--usage-field usage "input_tokens")
            (prompt-cache-episode-sequence episode)
            (conversation-next-sequence (application-conversation application))
            (prompt-cache-episode-history-characters episode)
            (application-idle-compaction--history-characters
             (conversation-input-items (application-conversation application))))))
  nil)

(-> application-idle-compaction-delay
    (application &key (:now real)) (option real))
(defun application-idle-compaction-delay
    (application &key (now (funcall *idle-compaction-clock-function*)))
  "Return seconds until eligible idle compaction, zero when due, or NIL.

Require a completed local request, unchanged conversation and provider, and
substantial summarizable history. Skip missed expiry windows and resumed history."
  (let* ((episode       (application-idle-compaction-episode application))
         (agent         (and (slot-boundp application 'agent) (application-agent application)))
         (configuration (and episode agent (application-configuration application))))
    (when (and configuration
               (config :idle-compaction-p configuration)
               (= (prompt-cache-episode-process-id episode) (current-process-id))
               (eq (prompt-cache-episode-conversation episode)
                   (application-conversation application))
               (eq (agent-conversation agent) (application-conversation application))
               (eq (prompt-cache-episode-provider episode) (agent-provider agent))
               (equal (prompt-cache-episode-model episode) (config :model configuration))
               (eql (prompt-cache-episode-sequence episode)
                    (conversation-next-sequence (application-conversation application)))
               (>= (or (prompt-cache-episode-prompt-tokens episode) 0)
                   *idle-compaction-minimum-tokens*)
               (>= (prompt-cache-episode-history-characters episode)
                   *idle-compaction-minimum-history-characters*))
      (let* ((retention (provider-prompt-cache-retention-seconds
                         (agent-provider agent) configuration))
             (age       (- now (prompt-cache-episode-started-at episode))))
        (when (and retention (> retention *idle-compaction-lead-seconds*)
                   (<= 0 age) (< age retention))
          (max 0 (- retention *idle-compaction-lead-seconds* age)))))))

(-> application-idle-compaction-consume (application prompt-cache-episode) boolean)
(defun application-idle-compaction-consume (application episode)
  "Consume a still-due EPISODE before any paid work; the caller owns admission.

Neither successful nor failed compaction can rearm this opportunity."
  (let ((delay (application-idle-compaction-delay application)))
    (when (and (eq episode (application-idle-compaction-episode application))
               delay (zerop delay))
      (setf (application-idle-compaction-episode application) nil)
      t)))


;;;; -- Native Controller Admission --

(-> application-input-controller--idle-compaction-ready-p
    (application-input-controller &key (:claimed-p boolean)) boolean)
(defun application-input-controller--idle-compaction-ready-p (controller &key claimed-p)
  "Check idle ownership with CONTROLLER's lock held; drafts are rechecked after reader pause."
  (let* ((application (application-input-controller-application controller))
         (ui          (application-ui application)))
    (and (not (application-input-controller-stopping-p controller))
         (not (application-input-controller-failure controller))
         (if claimed-p
             (eq (application-input-controller-active-work-kind controller) ':idle-compaction)
             (not (application-input-controller-active-p controller)))
         (not (application-input-controller-queued-work-paused-p controller))
         (not (application-input-controller-turn-cancellation-p controller))
         (not (application-localgroup-paused-p application))
         (not (application-localgroup-handoff-pending-p application))
         (null (application-input-controller-follow-up-edit-index controller))
         (every #'deque-empty-p
                (list (application-input-controller-initial-work-items controller)
                      (application-input-controller-work-items controller)
                      (application-input-controller-pending-apply-items controller)
                      (application-input-controller-steering-items controller)
                      (application-input-controller-steering-in-flight-items controller)))
         ui
         (zerop (length (line-editor-text (terminal-ui-editor ui))))
         (null (terminal-ui-image-attachments ui))
         t)))

(-> application-input-controller--idle-compaction-wait
    (application-input-controller) (option real))
(defun application-input-controller--idle-compaction-wait (controller)
  "Return the cache deadline delay, backing off while input or a draft blocks admission."
  (let ((delay (application-idle-compaction-delay
                (application-input-controller-application controller))))
    (when delay
      (if (and (zerop delay)
               (not (application-input-controller--idle-compaction-ready-p controller)))
          1
          delay))))

(-> application-input-controller--run-idle-compaction
    (application-input-controller prompt-cache-episode) null)
(defun application-input-controller--run-idle-compaction (controller episode)
  "Recheck input without a competing reader, then compact on the owning main thread."
  (let* ((application (application-input-controller-application controller))
         (admit
           (lambda ()
             (with-lock-held ((application-input-controller-lock controller))
               (let ((accepted-p
                       (and (application-input-controller--idle-compaction-ready-p
                             controller :claimed-p t)
                            (not (terminal-input-ready-p
                                  (terminal-ui-terminal (application-ui application))))
                            (application-idle-compaction-consume application episode))))
                 ;; Input arriving during selection ends this idle opportunity.
                 (when (eq episode (application-idle-compaction-episode application))
                   (setf (application-idle-compaction-episode application) nil))
                 accepted-p))))
         (admitted-p
           (if (application-input-controller-reader-live-p controller)
               (application-input-controller-call-with-reader-paused controller admit)
               (funcall admit))))
    (when admitted-p
      (application--call-with-model-error-handling
       application
       (lambda ()
         (application-present
          application
          (list (terminal-span ':hint "· compacting idle history before estimated cache expiry")))
         (application-compact application))
       :record-turn-aborted-p nil)))
  nil)

(-> application-idle-compaction-wake (application) null)
(defun application-idle-compaction-wake (application)
  "Recalculate the native idle deadline after a runtime setting change."
  (let ((controller (application-input-controller application)))
    (when controller
      (with-lock-held ((application-input-controller-lock controller))
        (sb-thread:condition-broadcast
         (application-input-controller-condition-variable controller)))))
  nil)
