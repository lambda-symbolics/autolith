(in-package #:autolith)

;;;; -- ACP Tool Presentation Tests --

(-> test-acp-observer-tool-title-previews () null)
(defun test-acp-observer-tool-title-previews ()
  "Preview principal arguments without control characters or unbounded titles."
  (dolist (case (list (list "lisp.eval" (json-object "compile" t "forms" #("(load-it)" "(+ 1 2)")) "(load-it) (+ 1 2)")
                     (list "shell.run" (json-object "async" t "command" "git status") "git status")
                     (list "resource.read" (json-object "uri" "conversation:current") "conversation:current")
                     (list "custom" (json-object "z" 9 "a" #(1 2)) "2 items")
                     (list "custom" (json-object "flag" (argo:json-false)) "false")))
    (destructuring-bind (name arguments preview) case
      (test-assert (search preview (acp-tool-title name arguments))
                   "the title previews the principal argument")))
  (dolist (arguments (list nil (json-object) (json-object "form" "")))
    (test-assert (string= "lisp.eval" (acp-tool-title "lisp.eval" arguments))
                 "empty arguments use the canonical tool name"))
  (let ((title (acp-tool-title "lisp.eval"
                               (json-object "form" (format nil "~C[31m~A~%tail"
                                                            #\Esc (make-string 100 :initial-element #\界))))))
    (test-assert (<= (text-cell-width title) (+ (text-cell-width "lisp.eval") 3 48))
                 "argument previews are bounded in display cells")
    (test-assert (not (find-if (lambda (character)
                                (or (< (char-code character) 32)
                                    (= (char-code character) 127)))
                              title))
                 "titles contain no control characters"))
  (test-call-with-function-replacements
   (list (list 'json-encode (lambda (&rest arguments)
                             (declare (ignore arguments))
                             (error "Compound title previews must not serialize their payload.")))
         (list 'sanitize-text (lambda (text &key single-line-p)
                               (declare (ignore single-line-p))
                               (test-assert (<= (length text) 192)
                                            "title sanitization consumes a bounded prefix")
                               text)))
   (lambda ()
     (acp-tool-title "lisp.eval" (json-object "form" (make-string 1000000 :initial-element #\x)))
     (acp-tool-title "custom" (json-object "items" (make-array 100000 :initial-element "large")))
     (acp-tool-title "lisp.eval" (json-object "forms" (make-array 4 :initial-element (make-string 1000000 :initial-element #\x))))))
  nil)

(-> test-acp-observer-submission-provenance () null)
(defun test-acp-observer-submission-provenance ()
  "Distinguish identical submissions by provenance, including after a refusal."
  (with-test-configuration (configuration)
    (let* ((service     (make-instance 'acp-service :configuration configuration))
           (session     (make-instance 'acp-session :service service :identifier "channel"
                                                  :configuration configuration :mode ':ask))
           (other       (make-instance 'acp-session :service service :identifier "channel"
                                                  :configuration configuration :mode ':ask))
           (submissions nil)
           (updates     nil)
           (refuse-p    t))
      (test-call-with-function-replacements
       (list (list 'acp-session-call
                   (lambda (session command &rest arguments)
                     (declare (ignore session command))
                     (push (getf arguments :submission) submissions)
                     (when refuse-p
                       (error 'agentcomms:acp-state-error :message "Submission refused."))
                     (list :busy-p nil)))
             (list 'acp-session-flush
                   (lambda (session)
                     (declare (ignore session))
                     nil))
             (list 'acp-session--send
                   (lambda (session update)
                     (push (cons session update) updates)
                     nil)))
       (lambda ()
         (handler-case (acp-session-prompt session "same")
           (agentcomms:acp-state-error ()
             nil))
         (test-assert (not (acp-session-prompt-active-p session))
                      "a refused submission releases the editor prompt")
         (setf refuse-p nil)
         (acp-session-prompt other "same")
         (let ((record (list :message :role ':user :content "same" :submission (first submissions))))
           (acp-session--record session record)
           (test-assert (= 1 (length updates))
                        "another client's identical text is visible after refusal")
           (acp-session--record other record)
           (test-assert (= 1 (length updates)) "the originating editor does not receive its own echo"))
         (acp-session-prompt session "same")
         (test-assert (not (equal (getf (first submissions) :id) (getf (second submissions) :id)))
                      "identical text receives distinct submission identities")
         (let ((record (list :message :role ':user :content "same" :submission (first submissions))))
           (acp-session--record session record)
           (acp-session--record other record)
           (test-assert (equal (list other session) (mapcar #'first updates))
                        "each client sees only the other client's identical submission")
           (test-assert (eq ':user-message-chunk
                            (agentcomms:acp-update-kind (acp-replay--record-update record 1)))
                        "replay includes prompts independently of their live origin"))
         (acp-session--record session '(:message :role :user :content "same"))
         (test-assert (= 3 (length updates)) "legacy records without provenance are displayed")
         (dolist (steer-p '(nil t))
           (acp-input--submit session (agentcomms:json-object "text" "same") steer-p)
           (test-assert (equal (if steer-p "steering" "queue")
                               (getf (first submissions) :source))
                        "extension submissions carry their actual source")
           (acp-session--record session
                                (list :message :role ':user :content "same"
                                      :submission (first submissions))))
         (test-assert (= 5 (length updates)) "queued and steering input are visible to their origin")
         (setf updates nil)
         (acp-session-submit session "same")
         (acp-session-submit other "same")
         (dolist (submission (subseq submissions 0 2))
           (dolist (recipient (list session other))
             (acp-session--record recipient
                                  (list :message :role ':user :content "same" :submission submission))))
         (test-assert (equal (list other session) (mapcar #'first updates))
                      "identical submissions admitted before either echo are distinguished by client")
         (let* ((record '(:message :role :user :content "#.(error \"replay evaluated text\")"))
                (before (copy-tree record)))
           (dotimes (repeat 2)
             (test-assert
              (equal (getf (rest record) :content)
                     (agentcomms:acp-content-text
                      (agentcomms:json-get (acp-replay--record-update record 1) "content")))
              "legacy replay treats reader syntax as literal content"))
           (test-assert (and (equal before record) (= 2 (length updates)))
                        "reading legacy history neither consumes records nor sends live updates"))))))
  nil)

(-> test-acp-attachment-history-boundary () null)
(defun test-acp-attachment-history-boundary ()
  "Deliver assistant and tool records appended during initial replay exactly once and in order."
  (with-test-configuration (configuration)
    (let* ((service      (make-instance 'acp-service :configuration configuration))
           (session      (make-instance 'acp-session :service service :identifier "channel"
                                                    :configuration configuration :mode ':ask))
           (updates      nil)
           (records      '((:message :seq 1 :role :user :content "question")
                           (:message :seq 2 :role :assistant :content "old reply")
                           (:tool-result :seq 3 :call-id "call" :status :ok :output "old result")))
           (conversation (conversation-create configuration)))
      (test-call-with-function-replacements
       (list (list 'conversation-replay-load
                   (lambda (&rest arguments) (declare (ignore arguments)) conversation))
             (list 'conversation-replay--map-records
                   (lambda (source function)
                     (declare (ignore source))
                     (dolist (record records) (funcall function record))
                     ;; New durable records are already visible to the reader, but lie
                     ;; beyond its captured boundary and arrive on the subscription.
                     (dolist (record '((:message :seq 4 :role :assistant :content "during attachment")
                                       (:tool-result :seq 5 :call-id "call" :status :ok :output "during result")))
                       (funcall function record)
                       (acp-session-event session
                                         (list :record  :sequence (+ 8 (getf (rest record) :seq))
                                                :record record)))
                     (acp-session-event session
                                       '(:status :sequence 14 :status :assistant-response-persisted
                                          :details (:text "during attachment")))
                     (acp-session-event session
                                       '(:status :sequence 15 :status :tool-call-completed
                                          :details (:call-id "call" :success-p t :output "during result")))
                     nil))
             (list 'acp-session--plan (lambda (recipient) (declare (ignore recipient)) nil))
             (list 'acp-session-flush (lambda (recipient) (declare (ignore recipient)) nil))
             (list 'acp-session--send
                   (lambda (recipient update) (declare (ignore recipient)) (push update updates) nil)))
       (lambda ()
         (acp-session-begin-attachment session)
         ;; A delayed listener for an already replayed record must not echo it.
         (acp-session-event session (list :record :sequence 11 :record (second records)))
         (acp-session-finish-attachment session '(:attached :sequence 10 :record-sequence 3) :replay-p t)
         (test-assert (equal '(:user-message-chunk :agent-message-chunk :tool-call-update
                               :agent-message-chunk :tool-call-update)
                             (mapcar #'agentcomms:acp-update-kind (reverse updates)))
                      "bounded history precedes each buffered assistant and tool result exactly once")
         (test-assert (= 15 (acp-session-event-sequence session)) "the delivered tail advances its event cursor")
         (test-assert (= 1 (acp-session-turn-sequence session)) "catch-up tool identities keep the replayed user turn")
         (acp-session-event session '(:status :sequence 16 :status :provider-request-started :details nil))
         (acp-session-event session '(:text :sequence 17 :delta "live reply"))
         (test-assert (eq ':agent-message-chunk (agentcomms:acp-update-kind (first updates)))
                      "the next provider request resumes live streaming")
         (test-assert (= 6 (length updates)) "going live does not replay the buffered transcript")))))
  nil)
