;;;; -- Optional Agent Protocol Presentation --

(in-package #:autolith)

(defclass agent-protocol-observer (agent-observer)
  ((driver :initarg :driver :reader agent-protocol-observer-driver
           :documentation "The established sole-reader driver; output never receives frames.")
   (turn-id :initarg :turn-id :reader agent-protocol-observer-turn-id
            :documentation "A trusted caller function returning the current turn ID or NIL.")
   (lock :initform      (bordeaux-threads:make-lock "agent protocol presentation")
         :reader        agent-protocol-observer-lock
         :documentation "Keeps one presentation's bounded chunks contiguous."))
  (:documentation "A presentation-only observer using existing one-way Output frames.
All frames are untrusted PROGRESS text. Labels carry display context, not parent
turn authority. Authorization methods use the base observer's denial behavior.
The client write timeout and parent consumer apply backpressure. There is no
queue, output acknowledgment, private terminal access or additional reader."))

(-> make-agent-protocol-observer (agent-protocol-driver &key (:turn-id function))
    (values agent-protocol-observer &optional))
(defun make-agent-protocol-observer (driver &key (turn-id (constantly nil)))
  "Make a presentation observer for DRIVER. TURN-ID reads trusted lifecycle state."
  (values (make-instance 'agent-protocol-observer :driver driver :turn-id turn-id)))

(-> agent-protocol-observer--label (string) string)
(defun agent-protocol-observer--label (text)
  "Keep a short, inert label. Never interpret a label as an identifier or command."
  (map 'string (lambda (character)
                 (if (or (alphanumericp character) (find character "-_.:/"))
                     character
                     #\?))
       (subseq text 0 (min 128 (length text)))))

(-> agent-protocol-observer--emit (agent-protocol-observer string string) null)
(defun agent-protocol-observer--emit (observer kind text)
  "Send bounded PROGRESS chunks. A failure ends this emission without retry.
The sole-reader driver remains responsible for all peer frames."
  (bordeaux-threads:with-lock-held ((agent-protocol-observer-lock observer))
    (let* ((driver  (agent-protocol-observer-driver observer))
           (turn-id (funcall (agent-protocol-observer-turn-id observer))))
      (unless (or (null turn-id) (and (stringp turn-id) (<= (length turn-id) 256)))
        (error 'agent-protocol-error :reason ':invalid-output-turn))
      (let ((prefix (format nil "[turn ~A type ~A] " (or turn-id "none")
                            (agent-protocol-observer--label kind))))
        (loop for start from 0 below (max 1 (length text)) by 1024
              do (agent-protocol-output
                  (agent-protocol-driver-client driver) ':progress
                  (concatenate 'string prefix (subseq text start (min (+ start 1024) (length text)))))))))
  nil)

(-> agent-protocol-observer--details (t) string)
(defun agent-protocol-observer--details (details)
  "Summarize common scalar status details without arbitrary object printing.
Bound collection traversal and text length, including malformed internal data."
  (with-output-to-string (stream)
    (labels ((emit (key value)
               (when (and (or (keywordp key) (stringp key))
                          (or (stringp value) (keywordp value) (typep value 'fixnum)))
                 (write-char #\Space stream)
                 (write-string (agent-protocol-observer--label
                                (if (stringp key) key (symbol-name key))) stream)
                 (write-char #\= stream)
                 (typecase value
                   (string
                    (write-string (subseq value 0 (min 512 (length value))) stream))
                   (symbol
                    (write-string (symbol-name value) stream))
                   (fixnum
                    (format stream "~D" value))))))
      (typecase details
        (hash-table
         (loop for key in '("name" "call_id" "namespace" "message" "status")
               do (emit key (gethash key details))))
        (list
         (loop for tail = details then (rest (rest tail)) repeat 8
               while (and (consp tail) (consp (rest tail)))
               do (emit (first tail) (second tail))))))))

(defmethod agent-observer-text ((observer agent-protocol-observer) (text string))
  "Publish text as transient presentation, never as a durable assistant record."
  (agent-protocol-observer--emit observer "text" text))

(defmethod agent-observer-reasoning ((observer agent-protocol-observer) (text string))
  "Publish visible reasoning as untrusted presentation."
  (agent-protocol-observer--emit observer "reasoning" text))

(defmethod agent-observer-status ((observer agent-protocol-observer) status details)
  "Publish bounded status and tool lifecycle details through the same output path."
  (agent-protocol-observer--emit
   observer "status"
   (concatenate 'string
                (if (keywordp status)
                    (agent-protocol-observer--label (string-downcase status))
                    "unknown")
                (agent-protocol-observer--details details))))
