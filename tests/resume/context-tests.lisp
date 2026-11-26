(in-package #:autolith)

;;;; -- Resumed Conversation Notice Tests --

(-> resume-context-tests--contribution
    (configuration conversation)
    (option context-contribution))
(defun resume-context-tests--contribution (configuration conversation)
  "Return the resumed-conversation contribution for CONVERSATION, if active."
  (find "resumed-conversation"
        (context-delivery-contributions
         (context-resolve-request configuration conversation #()))
        :key  #'context-contribution-identifier
        :test #'string=))

(-> test-resume-context () null)
(defun test-resume-context ()
  "Test the one-turn notice that pre-restart background work is gone."
  (let* ((configuration                 (test-configuration))
         (conversation                  (conversation-create configuration
                                                             :identifier "resume-note"))
         (*context-contributors*        nil)
         (*context-last-deliveries*     (make-hash-table :test #'equal))
         (*context-last-delivery-order* nil))
    (register-context-contributor "resumed-conversation"
                                  'resume-context
                                  :source ':built-in)
    (conversation-append-user-message conversation "start a long job")
    (test-assert
     (null (resume-context-tests--contribution configuration conversation))
     "a freshly created conversation carries no resume notice")
    (let ((resumed (conversation-load-by-id configuration "resume-note")))
      (test-assert (conversation-resumed-p resumed)
                   "loading from disk marks the conversation resumed")
      (conversation-append-user-message resumed "is the job done?")
      (let ((first (resume-context-tests--contribution configuration resumed)))
        (test-assert
         (and first
              (search "resumed this conversation"
                      (context-contribution-instruction first)))
         "the first request after a resume carries the notice"))
      (test-assert
       (not (null (resume-context-tests--contribution configuration resumed)))
       "the notice repeats through the first resumed turn")
      (test-assert
       (null (resume-context
              (make-instance 'request-context
                             :configuration   configuration
                             :conversation    resumed
                             :tool-namespaces #()
                             :compaction-p    t)))
       "a compaction request never carries the notice")
      (conversation-append-user-message resumed "thanks")
      (test-assert
       (null (resume-context-tests--contribution configuration resumed))
       "the notice retires once a later user turn begins")
      (test-assert
       (and (not (conversation-resumed-p resumed))
            (null (conversation-resume-note-turn resumed)))
       "retiring the notice clears the resumed state")))
  nil)
