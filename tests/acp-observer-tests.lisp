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
       (list (list 'channel-subscription-call
                   (lambda (session command &rest arguments)
                     (declare (ignore command))
                     (push (getf arguments :submission) submissions)
                     (when refuse-p
                       (error 'agentcomms:acp-state-error :message "Submission refused."))
                     (channel-subscription-event
                      session (list :work-finished :submission (getf arguments :submission)
                                    :outcome       ':done))
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
         (test-assert (not (channel-subscription-submission-active-p session))
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
         (channel-subscription-submit session "same")
         (channel-subscription-submit other "same")
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

(-> test-acp-prompt-submission-outcomes () null)
(defun test-acp-prompt-submission-outcomes ()
  "Await the exact queued submission despite an unrelated idle event before its reply."
  (with-test-configuration (configuration)
    (dolist (case '((:done :end-turn t) (:cancelled :cancelled t) (:failed :condition nil)))
      (destructuring-bind (work-outcome expected busy-p) case
        (let* ((service       (make-instance 'acp-service :configuration configuration))
               (session       (make-instance 'acp-session :service service :identifier "channel"
                                                         :configuration configuration :mode ':ask))
               (waiting       (sb-thread:make-semaphore))
               (finished      (sb-thread:make-semaphore))
               (original-wait (symbol-function 'condition-wait))
               (submission    nil)
               (result        nil)
               (thread        nil))
          (test-call-with-function-replacements
           (list (list 'channel-subscription-call
                       (lambda (recipient command &rest arguments)
                         (declare (ignore command))
                         (setf submission (getf arguments :submission))
                         (channel-subscription-event
                          recipient '(:work-finished :kind :message
                                      :submission (:id "other" :client "terminal")
                                      :outcome :done :idle-p t))
                         (list :busy-p busy-p)))
                 (list 'acp-session-flush (lambda (recipient) (declare (ignore recipient)) nil))
                 (list 'condition-wait
                       (lambda (condition-variable lock &rest arguments)
                         (when (eq condition-variable (channel-subscription-condition-variable session))
                           (sb-thread:signal-semaphore waiting))
                         (apply original-wait condition-variable lock arguments))))
           (lambda ()
             (unwind-protect
                  (progn
                    (setf thread
                          (make-thread
                           (lambda ()
                             (unwind-protect
                                  (setf result
                                        (handler-case (acp-session-prompt session "queued prompt")
                                          (agentcomms:acp-method-error () ':condition)
                                          (agentcomms:acp-state-error () ':closed)))
                               (sb-thread:signal-semaphore finished)))
                           :name "ACP exact submission test"))
                    (test-assert (sb-thread:wait-on-semaphore waiting :timeout 5)
                                 "unrelated idleness cannot finish the submission, even with a nonbusy reply")
                    (with-lock-held ((channel-subscription-lock session))
                      (test-assert (null (channel-subscription-submission-result session))
                                   "the unrelated work has no outcome for this prompt"))
                    (channel-subscription-event session
                                       (list :work-finished :sequence 1 :kind ':message
                                             :submission submission :outcome work-outcome :idle-p nil))
                    (test-assert (sb-thread:wait-on-semaphore finished :timeout 5)
                                 "a matching outcome completes without requiring channel idleness")
                    (test-assert (eq expected result) "the matching submission supplies the stop reason")
                    (channel-subscription-event session
                                       (list :work-finished :sequence 1 :kind ':message
                                             :submission submission :outcome ':done :idle-p t))
                    (test-assert (= 1 (channel-subscription-event-sequence session))
                                 "replayed completion cursors are consumed once"))
               (with-lock-held ((channel-subscription-lock session))
                 (setf (channel-subscription-closed-p session) t)
                 (sb-thread:condition-broadcast (channel-subscription-condition-variable session)))
               (when thread (sb-thread:join-thread thread :timeout 5 :default nil)))))))))
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
                       (channel-subscription-event session
                                         (list :record  :sequence (+ 8 (getf (rest record) :seq))
                                                :record record)))
                     (channel-subscription-event session
                                       '(:status :sequence 14 :status :assistant-response-persisted
                                          :details (:text "during attachment")))
                     (channel-subscription-event session
                                       '(:status :sequence 15 :status :tool-call-completed
                                          :details (:call-id "call" :success-p t :output "during result")))
                     nil))
             (list 'acp-session--plan (lambda (recipient) (declare (ignore recipient)) nil))
             (list 'acp-session-flush (lambda (recipient) (declare (ignore recipient)) nil))
             (list 'acp-session--send
                   (lambda (recipient update) (declare (ignore recipient)) (push update updates) nil)))
       (lambda ()
         (channel-subscription-begin-attachment session)
         ;; A delayed listener for an already replayed record must not echo it.
         (channel-subscription-event session (list :record :sequence 11 :record (second records)))
         (channel-subscription-finish-attachment session '(:attached :sequence 10 :record-sequence 3) :replay-p t)
         (test-assert (equal '(:user-message-chunk :agent-message-chunk :tool-call-update
                               :agent-message-chunk :tool-call-update)
                             (mapcar #'agentcomms:acp-update-kind (reverse updates)))
                      "bounded history precedes each buffered assistant and tool result exactly once")
         (test-assert (= 15 (channel-subscription-event-sequence session)) "the delivered tail advances its event cursor")
         (test-assert (= 1 (acp-session-turn-sequence session)) "catch-up tool identities keep the replayed user turn")
         (channel-subscription-event session '(:status :sequence 16 :status :provider-request-started :details nil))
         (channel-subscription-event session '(:text :sequence 17 :delta "live reply"))
         (test-assert (eq ':agent-message-chunk (agentcomms:acp-update-kind (first updates)))
                      "the next provider request resumes live streaming")
         (test-assert (= 6 (length updates)) "going live does not replay the buffered transcript")))))
  nil)

(-> test-acp-observer-catch-up-approval-identity () null)
(defun test-acp-observer-catch-up-approval-identity ()
  "Retain a suppressed catch-up call's identity for its subsequent approval."
  (with-test-configuration (configuration)
    (let* ((service         (make-instance 'acp-service :configuration configuration))
           (session         (make-instance 'acp-session :service service :identifier "channel"
                                                       :configuration configuration :mode ':ask))
           (updates         nil)
           (permission-call nil)
           (answers         nil))
      (setf (acp-session-catch-up-p session) t
            (acp-session-turn-sequence session) 23
            (acp-session-tool-call session) "previous-call")
      (test-call-with-function-replacements
       (list (list 'acp-session--send
                   (lambda (recipient update)
                     (declare (ignore recipient))
                     (push update updates)))
             (list 'acp-session-flush
                   (lambda (recipient) (declare (ignore recipient)) nil))
             (list 'agentcomms:agent-request-permission
                   (lambda (recipient identifier tool-call &rest options)
                     (declare (ignore recipient identifier options))
                     (setf permission-call tool-call)
                     (values ':selected "once")))
             (list 'channel-subscription-call
                   (lambda (recipient command &rest arguments)
                     (declare (ignore recipient))
                     (push (cons command arguments) answers)
                     nil)))
       (lambda ()
         (channel-subscription-event
          session '(:status :sequence 1 :status :tool-call-started
                    :details (:call-id "actual-call" :tool "shell.run"
                              :arguments "{\"command\":\"echo approved\"}")))
         (test-assert (null updates) "catch-up suppresses a duplicate live tool declaration")
         ;; Run approval synchronously to observe its wire report and answer without
         ;; introducing a permission-worker scheduling dependency into this case.
         (acp-session--approve
          session '(:id "approval" :kind :command :title "shell.run" :entry "echo approved"
                    :options ((:value "once") (:value "deny"))))
         (test-assert (equal "23:actual-call" (agentcomms:json-get permission-call "toolCallId"))
                      "permission refers to the actual suppressed call, not a stale or generated identity")
         (test-assert (equal '((:answer :approval "approval" :value "once")) answers)
                      "the editor's decision answers the original channel approval")
         (test-assert (null updates) "approval does not re-present the suppressed tool declaration")))))
  nil)
