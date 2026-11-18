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
                  (("(+ 1 2)" "(error \"second\")") nil "Form 2 of 2 failed")
                  (3 nil "forms as strings")))
    (destructuring-bind (forms succeeded-p fragment) case
      (let ((answer (client-evaluate-request (list :evaluation :id "e1" :forms forms))))
        (test-assert (and (equal (getf answer :evaluation) "e1")
                          (eq (getf answer :succeeded-p) succeeded-p)
                          (search fragment (getf answer :text)))
                     (format nil "~S answers ~:[a failure~;its result~] naming ~S"
                             forms succeeded-p fragment)))))
  (let ((evaluator (make-instance 'client-evaluator))
        (answers   nil))
    (client-evaluator-start evaluator '(:evaluation :id "e2" :forms ("(* 6 7)")))
    (test-assert (task-tests--wait-until
                  (lambda () (setf answers (client-evaluator-take evaluator)))
                  10)
                 "an evaluator finishes a request on a thread of its own")
    (test-assert (and (typep answers '(cons list null))
                      (typep (first answers) 'client-evaluation-reply)
                      (search "42" (getf (rest (first answers)) :text)))
                 "the finished answer is the packet the channel accepts")
    (test-assert (null (client-evaluator-take evaluator))
                 "an answer is taken once"))
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
       (let ((plain (localgroup-client-connect record :name "plain")))
         (unwind-protect
              (test-assert (eq (code "(+ 1 2)") ':no-client)
                           "a client that never said it evaluates is not asked")
           (localgroup-client-close plain)))
       (let ((connection (localgroup-client-connect record :name "lisp" :evaluates-p t))
             (gate       (sb-thread:make-semaphore)))
         (unwind-protect
              (progn
                (test-assert (search "42" (evaluate "(defparameter *client-evaluation-probe* 41)"
                                                    "(1+ *client-evaluation-probe*)"))
                             "the client's rendered result comes back")
                (multiple-value-bind (failure message) (code "(error \"boom\")")
                  (test-assert (and (eq failure ':failed) (search "boom" message))
                               "forms that fail in the client fail with the client's message"))
                (test-call-with-function-replacements
                 (list (list 'client-evaluate-request
                             (lambda (request)
                               (sb-thread:wait-on-semaphore gate :timeout 30)
                               (list :evaluation  (getf (rest request) :id)
                                     :succeeded-p t
                                     :text        "late"))))
                 (lambda ()
                   (let ((*client-evaluation-seconds* 0.5))
                     (test-assert (eq (code "(+ 1 2)") ':timeout)
                                  "a client that stays silent is given up on"))
                   (let ((controller (application-input-controller application)))
                     (setf (application-input-controller-turn-cancellation-p controller) t)
                     (unwind-protect
                          (test-assert (eq (code "(+ 1 2)") ':cancelled)
                                       "a cancelled turn stops waiting for the client")
                       (setf (application-input-controller-turn-cancellation-p controller) nil)))
                   (let* ((failure nil)
                          (asker   (make-thread
                                    (lambda () (setf failure (code "(+ 1 2)")))
                                    :name "client evaluation asker")))
                     (test-assert (task-tests--wait-until
                                   (lambda ()
                                     (client-hub-evaluations (application-client-hub application)))
                                   10)
                                  "the channel waits for the client's answer")
                     (localgroup-client-close connection)
                     (join-thread asker)
                     (test-assert (eq failure ':client-left)
                                  "a client that leaves ends the wait")))))
           (sb-thread:signal-semaphore gate 8)
           (localgroup-client-close connection)))
       (test-assert (null (client-hub-evaluations (application-client-hub application)))
                    "no finished or abandoned evaluation stays with the channel"))))
  nil)

(-> test-client-evaluation-terminal-client () null)
(defun test-client-evaluation-terminal-client ()
  "Test a channel evaluates forms in the controlling terminal client that says it evaluates."
  (client-protocol-tests--call
   (lambda (application record)
     (multiple-value-bind (socket stream)
         (daemon-connect (localgroup--record-port (rest record)))
       (let ((answerer nil))
         (unwind-protect
              (progn
                (daemon-write-packet
                 stream
                 (list :localgroup-request
                       :version   *daemon-protocol-version*
                       :token     (localgroup--record-token (rest record))
                       :operation ':attach
                       :arguments (list :mode ':control :rows 31 :columns 91 :styled-p nil
                                        :evaluates-p t)))
                (test-assert (eq (first (test-localgroup--read-packet stream)) ':attached)
                             "the terminal client attaches")
                (setf answerer
                      (make-thread
                       (lambda ()
                         (loop for packet = (handler-case (daemon-read-packet stream)
                                              (error ()
                                                nil))
                               while packet
                               do (when (eq (first packet) ':evaluation)
                                    (daemon-write-packet
                                     stream (cons :evaluated (client-evaluate-request packet))))))
                       :name "client evaluation terminal"))
                (test-assert (task-tests--wait-until
                              (lambda () (application-evaluating-attachment application))
                              10)
                             "the controlling terminal is the client that evaluates")
                (test-assert (search "\"client\""
                                     (application-evaluate-on-client
                                      application '("(string-downcase \"CLIENT\")")))
                             "the terminal client's rendered result comes back")
                (test-assert (eq (client-evaluation-tests--failure
                                  (lambda ()
                                    (application-evaluate-on-client application '("(error \"boom\")"))))
                                 ':failed)
                             "forms that fail in the terminal client fail the evaluation")
                (test-assert (notany (lambda (packet)
                                       (localgroup--accept-client-packet application packet))
                                     '((:evaluated :evaluation 3)
                                       (:evaluated :evaluation "none" :succeeded-p t :text "stray")
                                       (:evaluated . 3)
                                       (:other)))
                             "malformed and stray answers are taken by nobody"))
           ;; Shutting the socket down wakes the answerer's blocked read; closing alone does not.
           ; mallet:suppress no-ignore-errors -- the endpoint may have closed the socket already
           (ignore-errors (sb-bsd-sockets:socket-shutdown socket :direction ':io))
           (when answerer
             (join-thread answerer))
           ; mallet:suppress no-ignore-errors -- the endpoint may have closed the socket already
           (ignore-errors (sb-bsd-sockets:socket-close socket)))))))
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
