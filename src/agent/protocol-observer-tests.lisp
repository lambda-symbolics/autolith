;;;; -- Agent Protocol Presentation Checks --

(in-package #:autolith)

(-> run-agent-protocol-observer-tests () integer)
(defun run-agent-protocol-observer-tests ()
  "Exercise observer dispatch through serialized one-way frames with a live driver."
  (agent-protocol-test--peers
   (lambda (parent)
     (autolith-supervisor::parent-connection-handshake parent (agent-protocol-test--welcome))
     (let ((texts nil))
       (loop repeat 5
             do (let* ((envelope (autolith-supervisor::parent-connection-receive parent))
                       (output   (agent-wire:envelope.output envelope)))
                  (assert output)
                  (assert (zerop (agent-wire:envelope.reply-to envelope)))
                  (assert (eq ':progress (agent-wire:output.channel output)))
                  (assert (<= (length (agent-wire:output.text output)) 8192))
                  (push (agent-wire:output.text output) texts)))
       (setf texts (nreverse texts))
       (assert (every (lambda (text) (search "[turn stable-turn type " text)) texts))
       (assert (search "type text]" (first texts)))
       (assert (search "type reasoning] visible" (fourth texts)))
       (assert (search "type status] tool-started" (fifth texts)))
       (assert (search "NAME=tool" (fifth texts)))
       (assert (search "CALL-ID=call-1" (fifth texts)))
       (let ((done (autolith-supervisor::parent-connection-receive parent)))
         (assert (string= "done" (agent-wire:output.text (agent-wire:envelope.output done)))))))
   (lambda (client)
     (agent-protocol-handshake client)
     (let* ((driver   (agent-protocol-driver-start client))
            (observer (make-agent-protocol-observer driver :turn-id (constantly "stable-turn"))))
       (unwind-protect
           (progn
             (agent-observer-text observer (make-string 2050 :initial-element #\x))
             (agent-observer-reasoning observer "visible")
             (agent-observer-status observer ':tool-started '(:name "tool" :call-id "call-1"))
             (assert (null (agent-observer-take-steering observer)))
             (agent-protocol-output client ':diagnostic "done"))
         (agent-protocol-driver-close driver)
         (agent-protocol-driver-join driver)))))
  1)
