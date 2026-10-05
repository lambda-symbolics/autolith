(in-package #:autolith)

;;;; -- Resumed Conversation Notice --

(defparameter *resume-context-instruction*
    "Autolith resumed this conversation from disk. Inspect job.continuity to classify earlier unfinished work: owned live jobs may be reattached, compatible saved worker images reconstructed, explicitly safe specifications restarted, and other dead work needs a decision. Do not replay side effects without explicit authority and a fresh deduplication claim. Ordinary job operations are available for currently owned jobs; durable completed child results are readable through job.get."
  "The explicit continuity boundary shown on the first resumed turn.")

(-> resume-context--active-p (conversation) boolean)
(defun resume-context--active-p (conversation)
  "Return true while CONVERSATION's first resumed user turn is in progress.

The first request after loading records the current user-turn count. The
notice repeats through that turn's whole request loop and retires once a
later user turn begins."
  (block nil
    (unless (conversation-resumed-p conversation)
      (return nil))
    (let ((turns (conversation-user-turn-count conversation))
          (noted (conversation-resume-note-turn conversation)))
      (cond
        ((null noted)
         (setf (conversation-resume-note-turn conversation) turns)
         t)
        ((= noted turns)
         t)
        (t
         (setf (conversation-resumed-p conversation) nil
               (conversation-resume-note-turn conversation) nil)
         nil)))))

(-> resume-context (request-context) (option context-contribution))
(defun resume-context (request)
  "Tell the model once after resume to inspect explicit job continuity."
  (when (and (not (request-context-compaction-p request))
             (resume-context--active-p (request-context-conversation request)))
    (make-context-contribution
     :identifier "resumed-conversation"
     :instruction *resume-context-instruction*
     :priority 60
     :lifetime ':turn
     :class ':mandatory
     :deduplication-key "resumed-conversation")))

(register-context-contributor "resumed-conversation"
                              'resume-context
                              :source ':built-in)
