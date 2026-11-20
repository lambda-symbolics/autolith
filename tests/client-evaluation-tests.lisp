(in-package #:autolith)

;;;; -- Client Evaluation Tests --

(-> client-evaluation-tests--failure (function) (values (option keyword) (option string)))
(defun client-evaluation-tests--failure (function)
  "Call FUNCTION and return the code and message of the CLIENT-EVALUATION-ERROR it signals, or NIL."
  (handler-case (progn (funcall function) (values nil nil))
    (client-evaluation-error (condition)
      (values (tool-error-code condition) (autolith-error-message condition)))))

(-> test-client-evaluation-request () null)
(defun test-client-evaluation-request ()
  "Test a client turns an evaluation request into its answer, for forms that run and forms that fail."
  (dolist (case '((("(+ 1 2)") t "3")
                  (("(defparameter *client-evaluation-probe* 41)"
                    "(1+ *client-evaluation-probe*)")
                   t "42")
                  (("(error \"boom\")") nil "boom")
                  (("(+ 1 2)" "(error \"second\")") nil "Form 2 of 2 failed")))
    (destructuring-bind (forms succeeded-p fragment) case
      (let ((answer (client-evaluate-request (list :evaluation :id "e1" :forms forms))))
        (test-assert (and (equal (getf answer :evaluation) "e1")
                          (eq (getf answer :succeeded-p) succeeded-p)
                          (search fragment (getf answer :text)))
                     (format nil "~S answers ~:[a failure~;its result~] naming ~S"
                             forms succeeded-p fragment)))))
  (let* ((answers   nil)
         (evaluator (make-instance 'client-evaluator
                                   :delivery (lambda (packet)
                                               (push packet answers)))))
    (client-evaluator-start evaluator '(:evaluation :id "e2" :forms ("(* 6 7)")))
    (test-assert (task-tests--wait-until (lambda () (null *client-evaluator-active*)) 10)
                 "an evaluator finishes a request on a thread of its own")
    (test-assert (and (typep answers '(cons list null))
                      (client-evaluation-reply-p (first answers))
                      (search "42" (getf (rest (first answers)) :text)))
                 "the finished answer is delivered once, as the packet the channel accepts"))
  (let* ((answers    nil)
         (evaluator  (make-instance 'client-evaluator
                                    :delivery (lambda (packet)
                                                (push packet answers))))
         (connection (make-instance 'localgroup-client-connection))
         (evaluated  0))
    (test-call-with-function-replacements
     (list (list 'self-evaluate-sources
                 (lambda (&rest arguments) (declare (ignore arguments)) (incf evaluated) "unexpected")))
     (lambda ()
       (dolist (request '((:other :id "e" :forms ("(+ 1 2)"))
                          (:evaluation :forms ("(+ 1 2)"))
                          (:evaluation :id "" :forms ("(+ 1 2)"))
                          (:evaluation :id 3 :forms ("(+ 1 2)"))
                          (:evaluation :id "e" :id "other" :forms ("(+ 1 2)"))
                          (:evaluation :id "e" :forms 3)
                          (:evaluation :id "e" :forms ("(+ 1 2)" . "tail"))
                          (:evaluation :id "e" :forms ("(+ 1 2)") :restart 3)
                          (:evaluation :id "e" :forms ("(+ 1 2)") :restart-value nil)
                          (:evaluation :id "e" :forms)
                          (:evaluation . 3)))
         (test-assert (and (not (client-evaluation-request-p request))
                           (eq (client-evaluation-tests--failure
                                (lambda () (client-evaluate-request request))) ':invalid-request))
                      "Malformed or unidentified evaluation envelopes are refused before reading their forms.")
         (client-evaluator-start evaluator request)
         (localgroup-client--evaluate connection request))
       (test-assert (and (zerop evaluated) (null answers))
                    "Neither an evaluator nor a connection evaluates or answers malformed envelopes."))))
  (client-evaluation-tests--consecutive-requests)
  (client-evaluation-tests--abandon-handoff)
  (client-evaluation-tests--lifecycle)
  (client-evaluation-tests--stalled-writer)
  nil)

(-> client-evaluation-tests--consecutive-requests () null)
(defun client-evaluation-tests--consecutive-requests ()
  "Submit the next request after the first reply is visible but before its writer returns."
  (let* ((published (sb-thread:make-semaphore))
         (release   (sb-thread:make-semaphore))
         (finished  (sb-thread:make-semaphore))
         (reply     nil)
         (owner     (make-instance 'client-evaluator
                                   :delivery (lambda (packet)
                                               (cond
                                                 ((equal (getf (rest packet) :evaluation) "first")
                                                  (sb-thread:signal-semaphore published)
                                                  (sb-thread:wait-on-semaphore release :timeout 10))
                                                 ((equal (getf (rest packet) :evaluation) "second")
                                                  (setf reply (rest packet))
                                                  (sb-thread:signal-semaphore finished)))))))
    (unwind-protect
         (progn
           (client-evaluator-start owner '(:evaluation :id "first" :forms ("(+ 1 2)")))
           (test-assert (sb-thread:wait-on-semaphore published :timeout 5)
                        "The first answer is visible to the caller.")
           (client-evaluator-start owner '(:evaluation :id "second" :forms ("(error \"boom\")")))
           (client-evaluator-cancel owner "first")
           (test-assert (eq (client-evaluator-start owner '(:evaluation :id "third" :forms ("42"))) ':busy)
                        "A stalled writer admits at most one successor.")
           (sb-thread:signal-semaphore release)
           (test-assert (and (sb-thread:wait-on-semaphore finished :timeout 5)
                             (not (getf reply :succeeded-p)) (search "boom" (getf reply :text)))
                        (format nil "The consecutive request must run its own forms; received ~S." reply)))
      (sb-thread:signal-semaphore release)
      (client-evaluator-close owner)
      (test-assert (task-tests--wait-until (lambda () (null *client-evaluator-active*)) 5)
                   "The final request releases shared capacity.")))
  nil)

(-> client-evaluation-tests--abandon-handoff () null)
(defun client-evaluation-tests--abandon-handoff ()
  "Closing or cancelling a successor before its writer returns discards it without running Lisp."
  (dolist (action '(:close :cancel))
    (let* ((gate      (sb-thread:make-semaphore))
           (published (sb-thread:make-semaphore))
           (calls     0)
           (owner     (make-instance 'client-evaluator
                                     :delivery (lambda (packet)
                                                 (declare (ignore packet))
                                                 (sb-thread:signal-semaphore published)
                                                 (sb-thread:wait-on-semaphore gate :timeout 10)))))
      (test-call-with-function-replacements
       (list (list 'self-evaluate-sources
                   (lambda (&rest arguments) (declare (ignore arguments)) (incf calls) "answer")))
       (lambda ()
         (unwind-protect
              (progn
                (client-evaluator-start owner '(:evaluation :id "one" :forms ("1")))
                (test-assert (sb-thread:wait-on-semaphore published :timeout 5) "First answer published.")
                (test-assert (eq (client-evaluator-start owner '(:evaluation :id "two" :forms ("2"))) ':accepted)
                             "The same writer accepts a follow-up.")
                (ecase action
                  (:close
                   (client-evaluator-close owner))
                  (:cancel
                   (client-evaluator-cancel owner "two"))))
           (sb-thread:signal-semaphore gate)
           (test-assert (task-tests--wait-until (lambda () (null *client-evaluator-active*)) 5)
                        "The abandoned handoff releases capacity when the writer returns.")
           (client-evaluator-close owner))
         (test-assert (= calls 1) "Abandoned queued forms were never evaluated.")))))
  nil)

(-> test-client-evaluation-structured-client () null)
(defun test-client-evaluation-structured-client ()
  "Test a channel evaluates forms in a structured client that says it evaluates, and every way that fails."
  (client-protocol-tests--call
   (lambda (application record)
     (flet ((evaluate (&rest forms)
              (application-evaluate-on-client application forms))

            (code (&rest forms)
              (client-evaluation-tests--failure
               (lambda () (application-evaluate-on-client application forms)))))
       (test-assert (eq (code "(+ 1 2)") ':no-client)
                    "a channel without clients has nobody to evaluate")
       (client-evaluation-tests--peer-cancellation application record)
       (let ((plain (localgroup-client-connect record :name "plain")))
         (unwind-protect
              (test-assert (eq (code "(+ 1 2)") ':no-client)
                           "a client that never said it evaluates is not asked")
           (localgroup-client-close plain)))
       (let ((connection (localgroup-client-connect record :name "lisp" :evaluates-p t))
             (started    (sb-thread:make-semaphore))
             (gate       (sb-thread:make-semaphore)))
         (unwind-protect
              (progn
                (test-assert (search "42" (evaluate "(defparameter *client-evaluation-probe* 41)"
                                                    "(1+ *client-evaluation-probe*)"))
                             "the client's rendered result comes back")
                (multiple-value-bind (failure message) (code "(error \"boom\")")
                  (test-assert (and (eq failure ':failed) (search "boom" message))
                               "forms that fail in the client fail with the client's message"))
                (let* ((hub     (application-client-hub application))
                       (pending (make-instance 'client-evaluation
                                               :attachment (application-evaluating-attachment application)))
                       (forger  (localgroup-client-connect record :name "forger")))
                  (unwind-protect
                       (progn
                         (with-lock-held ((client-hub-lock hub))
                           (push pending (client-hub-evaluations hub)))
                         (test-assert
                          (not (getf (localgroup-client-call
                                      forger :evaluated :evaluation (client-evaluation-identifier pending)
                                      :succeeded-p t :text "forged") :accepted-p))
                          "A different structured connection cannot answer another attachment's evaluation.")
                         (dolist (fields '((:text "missing") (:succeeded-p :yes :text "truthy")))
                           (test-assert
                            (not (getf (apply #'localgroup-client-call connection :evaluated
                                              :evaluation (client-evaluation-identifier pending) fields) :accepted-p))
                            "Structured replies require an explicit boolean status."))
                         (test-assert
                          (and (null (client-evaluation-answer pending))
                               (getf (localgroup-client-call
                                      connection :evaluated :evaluation (client-evaluation-identifier pending)
                                      :succeeded-p nil :text "reported failure") :accepted-p)
                               (equal (client-evaluation-answer pending) '(nil "reported failure")))
                          "Only the originating attachment can answer, including an explicit false status."))
                    (with-lock-held ((client-hub-lock hub))
                      (setf (client-hub-evaluations hub) (remove pending (client-hub-evaluations hub))))
                    (localgroup-client-close forger)))
                (test-call-with-function-replacements
                 (list (list 'client-evaluate-request
                             (lambda (request)
                                (sb-thread:signal-semaphore started)
                               (sb-thread:wait-on-semaphore gate :timeout 30)
                               (list :evaluation  (getf (rest request) :id)
                                     :succeeded-p t
                                     :text        "late"))))
                 (lambda ()
                   (let ((*client-evaluation-seconds* 0.5))
                     (test-assert (eq (code "(+ 1 2)") ':timeout)
                                  "a client that stays silent is given up on"))
                   (test-assert (sb-thread:wait-on-semaphore started :timeout 5)
                                "The timed-out request occupied the evaluation worker.")
                   (sb-sys:with-deadline (:seconds 10)
                     (localgroup-client-call connection ':state))
                   (test-assert (with-lock-held (*client-evaluator-lock*)
                                  (null (client-evaluator-work
                                         (localgroup-client-connection-evaluator connection))))
                                "A negotiated cancellation detaches the timed-out work owner.")
                   (multiple-value-bind (failure message) (code "(+ 1 2)")
                     (test-assert (and (eq failure ':failed) (search "busy" message))
                                  "An abandoned worker refuses new work explicitly."))
                   (let ((hub        (application-client-hub application))
                         (controller (application-input-controller application)))
                     (dolist (action '(:cancelled :client-left))
                       (sb-thread:signal-semaphore gate)
                       (test-assert (task-tests--wait-until
                                     (lambda ()
                                       (with-lock-held (*client-evaluator-lock*)
                                         (null *client-evaluator-active*)))
                                     5)
                                    "The abandoned worker exits before the next request.")
                       (let* ((failure nil)
                              (asker   (make-thread
                                        (lambda ()
                                          (let ((*client-evaluation-seconds* 10))
                                            (setf failure (code "(+ 1 2)"))))
                                        :name "client evaluation asker")))
                         (unwind-protect
                              (progn
                                (test-assert (sb-thread:wait-on-semaphore started :timeout 5)
                                             "The peer starts the evaluation before the interruption.")
                                (ecase action
                                  (:cancelled
                                   (with-lock-held ((client-hub-lock hub))
                                     (setf (application-input-controller-turn-cancellation-p controller) t)))
                                  (:client-left
                                   (localgroup-client-close connection)))
                                (join-thread asker)
                                (test-assert (eq failure action)
                                             (format nil "An evaluation interrupted by ~S returned ~S." action failure))
                                (when (eq action ':cancelled)
                                  (sb-sys:with-deadline (:seconds 10)
                                    (localgroup-client-call connection ':state))))
                           (with-lock-held ((client-hub-lock hub))
                             (setf (application-input-controller-turn-cancellation-p controller) nil)))))))))
           (sb-thread:signal-semaphore gate 8)
           (localgroup-client-close connection)
           (test-assert (task-tests--wait-until
                         (lambda ()
                           (with-lock-held (*client-evaluator-lock*)
                             (null *client-evaluator-active*)))
                         5)
                        "Structured teardown leaves no cooperative worker running.")))
       (test-assert (null (client-hub-evaluations (application-client-hub application)))
                    "no finished or abandoned evaluation stays with the channel"))))
  nil)

(-> test-client-evaluation-terminal-client () null)
(defun test-client-evaluation-terminal-client ()
  "Test a channel evaluates forms in the terminal client its user is at, before any other client that evaluates."
  (client-protocol-tests--call
   (lambda (application record)
     (let ((terminal (test-localgroup--present record :evaluates-p t))
           (other    (localgroup-client-connect record :name "lisp" :evaluates-p t)))
       (unwind-protect
            (let ((controller (application-controlling-client application)))
              (test-assert (and controller
                                (channel-client-presents-p controller)
                                (eq (application-evaluating-attachment application)
                                    (channel-client-attachment controller)))
                           "the terminal client evaluates, though another client that evaluates attached after it")
              (test-assert (search "\"client\""
                                   (application-evaluate-on-client
                                    application '("(string-downcase \"CLIENT\")")))
                           "the terminal client's rendered result comes back")
              (test-assert (eq (client-evaluation-tests--failure
                                (lambda ()
                                  (application-evaluate-on-client application '("(error \"boom\")"))))
                               ':failed)
                           "forms that fail in the terminal client fail the evaluation")
              (dolist (answer '((:evaluation 3)
                                (:evaluation "none" :succeeded-p t :text "stray")
                                (:evaluation "none")))
                (test-assert (null (getf (apply #'localgroup-client-call terminal ':evaluated answer)
                                         :accepted-p))
                             (format nil "the answer ~S is taken by nobody" answer)))
              (test-call-with-function-replacements
               (list (list 'client-evaluate-request
                           (lambda (request)
                             (list :evaluation  (getf (rest request) :id)
                                   :succeeded-p t
                                   :text        "ok"
                                   :elapsed     3))))
               (lambda ()
                 (test-assert (search "ok" (application-evaluate-on-client application '("(+ 1 2)")))
                              "an answer with a field this version does not know is still an answer")))
               ;; Receiving the reply can precede the worker releasing its slot.
               (test-assert (task-tests--wait-until
                             (lambda ()
                               (with-lock-held (*client-evaluator-lock*)
                                 (null *client-evaluator-active*)))
                             5)
                            "The terminal evaluation releases shared capacity before switching attachments.")
               (localgroup-client-close terminal)
               (test-assert (task-tests--wait-until
                             (lambda ()
                               (and (application-evaluating-attachment application)
                                    (null (application-controlling-client application))))
                             10)
                            "the terminal relinquishes control after closing")
               (test-assert (search "3" (application-evaluate-on-client application '("(+ 1 2)")))
                            "once the terminal left, the client attached last evaluates"))
         (localgroup-client-close terminal)
         (localgroup-client-close other)))))
  nil)

(-> test-self-eval-client-target () null)
(defun test-self-eval-client-target ()
  "Test self.eval sends its forms to the client only when asked, and journals only the active image's."
  (with-test-configuration (configuration)
    (let* ((asked    nil)
           (registry (make-default-tool-registry))
           (observer (callback-agent-observer-create
                      :client-evaluation-callback
                      (lambda (sources &key restart-name restart-value-source)
                        (push (list sources restart-name restart-value-source) asked)
                        "Values:
from the client")))
           (context  (make-instance 'tool-context
                                    :configuration configuration
                                    :worker        nil
                                    :conversation  (conversation-create configuration
                                                                        :identifier "client-eval")
                                    :observer      observer)))
      (labels ((run (run-context &rest arguments)
                 "Execute self.eval with ARGUMENTS through the registry in RUN-CONTEXT."
                 (tool-registry-execute-call
                  registry
                  (json-object "namespace" "self"
                               "name" "eval"
                               "arguments" (json-encode (apply #'json-object arguments)))
                  run-context))

               (journaled ()
                 "Return how many records the mutation journal holds."
                 (length (mutation-journal-read-records configuration))))
        (let ((before (journaled))
              (result (run context "forms" (vector "(+ 1 2)") "target" "client"
                                   "restart" "CONTINUE")))
          (test-assert (and (tool-result-success-p result)
                            (search "from the client" (tool-result-content result)))
                       "target client returns what the client answered")
          (test-assert (equal asked '((("(+ 1 2)") "CONTINUE" nil)))
                       "the client receives the forms and the chosen restart")
          (test-assert (= (journaled) before)
                       "a client evaluation leaves the mutation journal alone"))
        (dolist (arguments (list (list "forms" (vector "(+ 1 2)"))
                                 (list "forms" (vector "(+ 1 2)") "target" "server")))
          (let ((before (journaled))
                (result (apply #'run context arguments)))
            (test-assert (and (tool-result-success-p result)
                              (search "3" (tool-result-content result))
                              (= (length asked) 1)
                              (> (journaled) before))
                         (format nil "~S evaluates in the active image and journals it"
                                 (rest (rest arguments))))))
        (test-assert (not (tool-result-success-p
                           (run context "forms" (vector "(+ 1 2)") "target" "browser")))
                     "an unknown target is refused")
        (let ((result (run (make-instance 'tool-context
                                          :configuration configuration
                                          :worker        nil
                                          :conversation  (tool-context-conversation context))
                           "forms" (vector "(+ 1 2)") "target" "client")))
          (test-assert (and (not (tool-result-success-p result))
                            (search "No attached client evaluates Lisp"
                                    (tool-result-content result)))
                       "without a presenting observer there is no client to ask")))))
  nil)

(-> client-evaluation-tests--abandoned-owner () list)
(defun client-evaluation-tests--abandoned-owner ()
  "Start gated work over a connection and return weak references to its owner and to it after teardown."
  (let* ((connection (make-instance 'localgroup-client-connection
                                    :socket nil
                                    :stream (make-string-output-stream)))
         (owner      (localgroup-client-connection-evaluator connection))
         (other      (make-instance 'client-evaluator))
         (request    '(:evaluation :id "abandoned" :forms ("(values)"))))
    (localgroup-client--evaluate connection request)
    (client-evaluator-cancel other "abandoned")
    (test-assert (eq owner (client-evaluator-work-owner *client-evaluator-active*))
                 "Cancellation from another attachment cannot abandon this owner.")
    (client-evaluator-cancel owner "abandoned")
    (localgroup-client-close connection)
    (list (sb-ext:make-weak-pointer owner)
          (sb-ext:make-weak-pointer connection))))

(-> client-evaluation-tests--lifecycle () null)
(defun client-evaluation-tests--lifecycle ()
  "Check global admission, abandoned-owner collection, busy replies and eventual slot release."
  (let ((gate    (sb-thread:make-semaphore))
        (started (sb-thread:make-semaphore))
        (calls   0))
    (test-call-with-function-replacements
     (list (list 'self-evaluate-sources
                 (lambda (&rest arguments)
                   (declare (ignore arguments))
                   (incf calls)
                   (sb-thread:signal-semaphore started)
                   (sb-thread:wait-on-semaphore gate :timeout 10)
                   (make-string (* 2 *self-evaluation-character-limit*) :initial-element #\x))))
     (lambda ()
       (unwind-protect
            (let* ((weak         nil)
                   (control      (make-instance 'client-evaluator))
                   (control-weak (sb-ext:make-weak-pointer control))
                   (replies      nil)
                   (other        (make-instance 'client-evaluator
                                                :delivery (lambda (packet)
                                                            (push packet replies))))
                   (broken       (make-instance 'client-evaluator
                                                :delivery (lambda (packet)
                                                            (declare (ignore packet))
                                                            (error "The transport is gone.")))))
              (join-thread
               (make-thread (lambda ()
                              (setf weak (client-evaluation-tests--abandoned-owner))
                              nil)
                            :name "evaluation owner producer"))
              (test-assert (sb-thread:wait-on-semaphore started :timeout 5)
                           "The original image worker is running after its attachment leaves.")
              (test-assert (and *client-evaluator-active*
                                (null (client-evaluator-work-owner *client-evaluator-active*)))
                           "Abandoned work holds capacity without a delivery owner.")
              (sb-ext:gc :full t)
              (test-assert (eq control (sb-ext:weak-pointer-value control-weak))
                           "The positive control deliberately retains its owner across collection.")
              (test-assert (every (lambda (pointer) (null (sb-ext:weak-pointer-value pointer))) weak)
                           "A blocked worker retains neither its owner nor its connection.")
              (dotimes (index 3)
                (test-assert
                 (eq (client-evaluator-start
                      other (list :evaluation :id (format nil "busy-~D" index) :forms '("(values)")))
                     ':busy)
                 "New requests are refused as busy instead of starting more workers."))
              (test-assert (and (= calls 1)
                                (= 3 (length replies))
                                (every (lambda (packet)
                                         (and (client-evaluation-reply-p packet)
                                              (null (getf (rest packet) :succeeded-p))))
                                       replies)
                                (not (client-evaluator-closed-p other)))
                           "Each refused request is answered as busy through its owner's delivery.")
              (client-evaluator-start broken '(:evaluation :id "broken" :forms ("(values)")))
              (test-assert (client-evaluator-closed-p broken)
                           "A delivery that fails closes its owner."))
         (sb-thread:signal-semaphore gate)
         (test-assert (task-tests--wait-until (lambda () (null *client-evaluator-active*)) 5)
                      "Only actual worker exit releases shared capacity.")))))
  nil)

(defclass client-evaluation-tests-output (trivial-gray-streams:fundamental-character-output-stream)
  ((gate :initarg :gate :reader client-evaluation-tests-output-gate
         :documentation "A gate that models stalled output after a partial frame.")
   (started :initarg :started :reader client-evaluation-tests-output-started
            :documentation "Notifies the test that a frame write began.")
   (writes :initform 0 :accessor client-evaluation-tests-output-writes
           :documentation "Number of characters accepted before interruption."))
  (:documentation "A deadline-aware partial writer for transport retirement tests."))

(defmethod trivial-gray-streams:stream-write-char ((stream client-evaluation-tests-output) character)
  "Accept the first character and block until the deadline or test cleanup."
  (incf (client-evaluation-tests-output-writes stream))
  (sb-thread:signal-semaphore (client-evaluation-tests-output-started stream))
  (sb-thread:wait-on-semaphore (client-evaluation-tests-output-gate stream) :timeout 10)
  character)

(-> client-evaluation-tests--stalled-writer () null)
(defun client-evaluation-tests--stalled-writer ()
  "Exercise a partial-frame timeout, queued writers and another connection's reader and close."
  (let* ((gate            (sb-thread:make-semaphore))
         (started         (sb-thread:make-semaphore))
         (stream          (make-instance 'client-evaluation-tests-output :gate gate :started started))
         (connection      (make-instance 'localgroup-client-connection :socket nil :stream stream))
         (other           (make-instance 'localgroup-client-connection :socket nil
                                        :stream                                (make-string-input-stream
                                                                                (image-daemon:daemon-packet-string '(:reply :id 7 :ok t)))))
         (writer          nil)
         (queued          nil)
         (queued-failed-p nil))
    (setf (gethash 7 (localgroup-client-connection-replies other)) nil)
    (unwind-protect
         (progn
           (setf writer
                 (make-thread
                  (lambda ()
                    (let ((*localgroup-client-evaluation-write-seconds* 0.25))
                      (localgroup-client--evaluation-answer
                       connection '(:evaluated :evaluation "write" :succeeded-p t :text "answer"))))
                  :name "evaluation partial writer"))
           (test-assert (sb-thread:wait-on-semaphore started :timeout 5)
                        "The evaluation answer stalled after beginning its frame.")
           (setf queued
                 (make-thread
                  (lambda ()
                    (handler-case (localgroup-client--write-frame connection "second-frame")
                      (localgroup-error () (setf queued-failed-p t))))
                  :name "evaluation queued writer"))
           (sb-sys:with-deadline (:seconds 2)
             (client-evaluator-close (make-instance 'client-evaluator))
             (localgroup-client--read other nil)
             (localgroup-client-close other))
           (test-assert (and (localgroup-client-connection-closed-p other)
                             (gethash 7 (localgroup-client-connection-replies other)))
                        "A stalled writer holds neither shared admission nor another reader's close path.")
           (join-thread writer)
           (join-thread queued)
           (test-assert (and queued-failed-p (= 1 (client-evaluation-tests-output-writes stream))
                             (localgroup-client-connection-write-failed-p connection)
                             (localgroup-client-connection-closed-p connection)
                             (client-evaluator-closed-p (localgroup-client-connection-evaluator connection))
                             (zerop (hash-table-count (localgroup-client-connection-replies connection))))
                        "The interrupted frame retires before any queued writer, without an ACK entry."))
      (sb-thread:signal-semaphore gate 100)
      (when writer (join-thread writer))
      (when queued (join-thread queued))
      (localgroup-client-close connection)
      (localgroup-client-close other)))
  nil)

(-> client-evaluation-tests--peer-cancellation (application cons) null)
(defun client-evaluation-tests--peer-cancellation (application record)
  "Timeout old and new structured peers and verify cancellation is explicitly negotiated."
  (dolist (supports-p '(nil t))
    (multiple-value-bind (socket stream) (daemon-connect (localgroup--record-port (rest record)))
      (unwind-protect
           (progn
             (daemon-write-packet
              stream (list :localgroup-request :version *daemon-protocol-version*
                           :token (localgroup--record-token (rest record)) :operation ':client
                           :arguments          (append (list :name "compatibility" :evaluates-p t)
                                                       (when supports-p (list :cancels-evaluation-p t)))))
             (test-assert (eq (first (daemon-read-response stream ':client)) ':attached)
                          "An evaluation-only old peer and a cancellation-aware peer both attach.")
             (test-assert (task-tests--wait-until
                           (lambda () (application-evaluating-attachment application)) 5)
                          "The peer's evaluation capability is admitted.")
             (test-assert (eq (attachment-cancels-evaluation-p
                               (application-evaluating-attachment application)) supports-p)
                          "Cancellation support belongs to the exact advertised attachment.")
             (let ((*client-evaluation-seconds*      0.05)
                   (*client-evaluation-poll-seconds* 0.01))
               (test-assert (eq (client-evaluation-tests--failure
                                 (lambda () (application-evaluate-on-client application '("(values)"))))
                               ':timeout)
                            "A peer that never answers releases its server evaluation state on timeout."))
             (test-assert (null (client-hub-evaluations (application-client-hub application)))
                          "Timeout removes server pending state for both protocol generations.")
             (sb-sys:with-deadline (:seconds 2)
               (let ((request (daemon-read-packet stream)))
                 (test-assert (eq (first request) ':evaluation) "The peer receives its evaluation request.")
                 (if supports-p
                     (test-assert (equal (daemon-read-packet stream)
                                         (list :cancel-evaluation :id (getf (rest request) :id)))
                                  "New peers receive best-effort cancellation for that request alone.")
                     (progn
                       (sleep 0.05)
                       (test-assert (not (listen stream))
                                    "Old peers receive no cancellation packet or unknown event."))))))
        (sb-bsd-sockets:socket-shutdown socket :direction ':io)
        (sb-bsd-sockets:socket-close socket)
        (test-assert (task-tests--wait-until
                      (lambda () (null (application-evaluating-attachment application))) 5)
                     "The compatibility peer leaves before admitting the next peer."))))
  nil)
