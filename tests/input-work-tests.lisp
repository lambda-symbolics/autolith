(in-package #:autolith)

;;;; -- Durable Input Payloads --

(-> test-input-work-payload-codec () null)
(defun test-input-work-payload-codec ()
  "Exercise direct payloads with images, identified steering and native work."
  (let* ((image    (merge-pathnames "input.png" (uiop:temporary-directory)))
         (input    (user-message-input-create :text "image" :image-pathnames (list image)))
         (state    (list :active-work                     (list ':native-lisp "(error \"unevaluated\")")
                         :active-work-identifier          "active"
                         :steering-in-flight-items
                         (list (agent-steering-input-create :identifier "steering" :content input))
                         :steering-items                  (list input)
                         :work-items                      (list (list ':message input)
                                                                (list ':native-lisp "(values)")
                                                                (list ':command "/help"))
                         :steering-promotion-prefix-count 2))
         (payload  (input-state->payload state))
         (restored (input-state-from-payload payload)))
    (test-assert restored "a payload is decodable without snapshot or conversation identity")
    (test-assert (equal payload (input-state->payload restored))
                 "direct payloads preserve images, identified work and queue order")
    (test-assert (= 6 (input-state-count restored))
                 "the input model counts active, in-flight, steering and queued input")
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
  nil)
