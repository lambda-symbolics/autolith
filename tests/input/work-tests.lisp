(in-package #:autolith)

;;;; -- Durable Input Payloads --

(-> test-input-work-payload-codec () null)
(defun test-input-work-payload-codec ()
  "Round-trip input attachments, queue provenance, steering and native work."
  (let* ((image      (merge-pathnames "input.png" (uiop:temporary-directory)))
         (submission '(:id "submission" :client "editor" :source :queue))
         (input      (user-message-input-create :text "image" :image-pathnames (list image)
                                                :submission submission))
         (state      (list :active-work
                           (list ':native-lisp "(error \"unevaluated\")" :submission submission)
                           :active-work-identifier          "active"
                           :steering-in-flight-items
                           (list (agent-steering-input-create :identifier "steering" :content input
                                                              :operation-request '(:source "another channel")))
                           :steering-items                  (list input)
                           :work-items
                           (list (input-work-from-input input)
                                 (list ':native-lisp "(values)" :submission submission)
                                (input-work-from-input
                                 (user-message-input-create :text "(+ 1 2)" :submission submission)))
                           :steering-promotion-prefix-count 2))
         (payload    (input-state->payload state))
         (restored   (input-state-from-payload payload)))
    (test-assert restored "a payload is decodable without snapshot or conversation identity")
    (test-assert (equal payload (input-state->payload restored))
                 "direct payloads preserve images, identified work and queue order")
    (test-assert (= 6 (input-state-count restored))
                 "the input model counts active, in-flight, steering and queued input")
    (dolist (work (cons (getf restored :active-work) (getf restored :work-items)))
      (test-assert (equal submission (getf (nthcdr 2 work) :submission))
                   "native, prose and Lisp work retain submission provenance"))
    (let ((form (input-state->form (list* :snapshot-identifier "snapshot" state) "conversation")))
      (test-assert (equal payload
                          (input-state->payload (input-state-from-form form "conversation")))
                   "the pending envelope uses the same payload codec")
      (test-assert (null (input-state-from-form form "other"))
                   "pending envelopes enforce their conversation identity"))
    (let ((bad (copy-tree payload)))
      (setf (getf (getf bad :native-work) :steering-promotion-prefix-count) 100)
      (test-assert (null (input-state-from-payload bad))
                   "invalid native queue metadata is rejected")))
  (dolist (form '((:lisp "(values)" :submission (:id "id"))
                  (:message "hello" :unexpected (:id "id" :client "client" :source :prompt))))
    (test-assert (null (input-work-from-form form)) "invalid work provenance is rejected"))
  (test-assert (equal (user-message-input-submission
                       (user-message-input-create
                        :text "hello" :submission '(:id "id" :client "client" :source "steering")))
                      '(:id "id" :client "client" :source :steering))
               "a source an older client or record spelled as a string is read as its keyword")
  (let* ((path       (merge-pathnames "legacy workspace/" (uiop:temporary-directory)))
         (provenance '(:id "legacy" :client "editor" :source :steering))
         (invocation '(:application-command-invocation :version 1 :command "cwd"
                       :arguments ("relative/")))
         (prepared   (list :command-message-input :version 1 :text "(cwd)"
                           :invocation invocation :phase ':prepared :prepared-arguments (list path)
                           :submission provenance))
         (pending    (list :command-message-input :version 1 :text "(cwd)"
                           :invocation invocation :phase ':pending))
         (done       (list :command-message-input :version 1 :text "(cwd)"
                           :invocation invocation :phase ':committed :notification "Workspace changed."))
         (payload    (list :active-work                     nil
                           :steering-in-flight              (list (list :identifier "in-flight" :input prepared))
                           :steering                        (list pending "prose steering")
                           :work                            (list (list ':message "queued prose") (list ':command done :submission provenance))
                           :steering-promotion-prefix-count 1
                           :native-work                     '(:active-work nil :items ((:position 1 :work (:native-lisp "(values)")))
                                                              :steering-promotion-prefix-count 2)))
         (snapshot   (input-state-from-form
                      (list* :pending-inputs :version 2 :snapshot-identifier "snapshot"
                             :conversation-id "conversation" payload)
                      "conversation"))
         (capture    (application-recovery-input-vault--capture-from-form
                      (list* :id "capture" :captured-at 1 payload) path))
         (work       (getf snapshot :work-items)))
    (test-assert (and snapshot capture (equal work (getf capture :work-items)))
                 "pending snapshots and vault captures upgrade the same mixed queue")
    (test-assert (equal (getf snapshot :steering-items) '("prose steering"))
                 "upgrading old commands keeps prose in the steering lane")
    (test-assert (equal (subseq work 0 2) '((:message "queued prose") (:native-lisp "(values)")))
                 "already promoted work keeps its order when legacy commands are restored")
    (test-assert (equal (mapcar (lambda (entry) (self-read-form (second entry) :read-eval nil)) (nthcdr 2 work))
                        (list (list 'cwd path) '(cwd "relative/") ''"Workspace changed."))
                 "recovery uses stored arguments and does not replay committed work")
    (test-assert (and (equal (getf (nthcdr 2 (third work)) :submission) provenance)
                      (equal (getf (nthcdr 2 (fifth work)) :submission) provenance))
                 "upgraded work retains provenance from both legacy record locations"))
  nil)
