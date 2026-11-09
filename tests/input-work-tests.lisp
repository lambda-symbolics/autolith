(in-package #:autolith)

;;;; -- Durable Input Payloads --

(-> test-input-work-payload-codec () null)
(defun test-input-work-payload-codec ()
  "Round-trip input attachments, queue provenance, steering and native work."
  (let* ((image      (merge-pathnames "input.png" (uiop:temporary-directory)))
         (submission '(:id "submission" :client "editor" :source "queue"))
         (input      (user-message-input-create :text "image" :image-pathnames (list image)
                                                :submission submission))
         (state      (list :active-work
                           (list ':native-lisp "(error \"unevaluated\")" :submission submission)
                           :active-work-identifier          "active"
                           :steering-in-flight-items
                           (list (agent-steering-input-create :identifier "steering" :content input))
                           :steering-items                  (list input)
                           :work-items
                           (list (input-work-from-input input)
                                 (list ':native-lisp "(values)" :submission submission)
                                 (input-work-from-input
                                  (user-message-input-create :text "/help" :submission submission))
                                 (input-work-from-input
                                  (user-message-input-create :text "(+ 1 2)" :submission submission)))
                           :steering-promotion-prefix-count 2))
         (payload    (input-state->payload state))
         (restored   (input-state-from-payload payload)))
    (test-assert restored "a payload is decodable without snapshot or conversation identity")
    (test-assert (equal payload (input-state->payload restored))
                 "direct payloads preserve images, identified work and queue order")
    (test-assert (= 7 (input-state-count restored))
                 "the input model counts active, in-flight, steering and queued input")
    (dolist (work (cons (getf restored :active-work) (getf restored :work-items)))
      (test-assert (equal submission (getf (nthcdr 2 work) :submission))
                   "native, prose, command and Lisp work retain submission provenance"))
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
                  (:command "/help" :submission (:id "id" :client "client" :source "unknown"))
                  (:message "hello" :unexpected (:id "id" :client "client" :source "prompt"))))
    (test-assert (null (input-work-from-form form)) "invalid work provenance is rejected"))
  nil)
