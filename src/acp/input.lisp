(in-package #:autolith)

;;;; -- Steering and Queued Input --

;;; The editor's steering and follow-up prose go to the channel as ordinary
;;; submissions: steering reaches a running turn at its next safe boundary,
;;; a follow-up waits its turn in the channel's queue.

(-> acp-input--submit (acp-session hash-table boolean) hash-table)
(defun acp-input--submit (session params steer-p)
  "Submit the prose in PARAMS to SESSION's channel, steering a running turn when STEER-P."
  (let ((text (agentcomms:acp-field params "text" :type ':string :required-p t)))
    (unless (non-empty-string-p (string-trim '(#\Space #\Tab #\Newline #\Return) text))
      (agentcomms:acp-invalid-params "Input text must not be blank."))
    (when (> (length text) 262144)
      (agentcomms:acp-invalid-params "Input text exceeds the character limit."))
    (with-lock-held ((channel-subscription-lock session))
      (when (channel-subscription-closed-p session)
        (error 'acp-extension-unavailable :reason "The session is closing.")))
    (let ((reply (channel-subscription-submit session text :steer-p steer-p
                                     :source                        (if steer-p "steering" "queue"))))
      (agentcomms:json-object "accepted" t
                              "delivery" (if (and steer-p (getf reply :busy-p)) "steering" "queued")))))

(-> acp-extension-steer (acp-session hash-table) hash-table)
(defun acp-extension-steer (session params)
  "Submit primary prose for the running turn's next safe boundary."
  (acp-input--submit session params t))

(-> acp-extension-queue (acp-session hash-table) hash-table)
(defun acp-extension-queue (session params)
  "Append a primary prose prompt to the channel's follow-up queue."
  (acp-input--submit session params nil))
