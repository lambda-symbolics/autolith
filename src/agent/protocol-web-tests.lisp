;;;; -- Serialized Parent Web Tool Checks --

(in-package #:autolith)

(-> agent-web-test--roundtrip (keyword) null)
(defun agent-web-test--roundtrip (case)
  "Call real web tools over the scoped protocol, with synthetic parent credentials and HTTP."
  (let ((handler-calls 0) (http-calls 0) (ran nil) (problem nil) (retained nil))
    (uiop:with-temporary-file (:pathname path :stream stream)
      (close stream)
      (delete-file path)
      (unwind-protect
           (agent-protocol-test--peers
            (lambda (parent)
              (let ((welcome (agent-session-test--welcome)))
                (setf (agent-wire:welcome.features welcome) '("session.v1" "application.v1"))
                (autolith-supervisor::parent-connection-handshake parent welcome))
              (let* ((application (autolith-supervisor::parent-web-test--application))
                     (service
                       (autolith-supervisor::make-parent-application-operation-service
                        (list
                         (list ':web
                               (lambda (arguments cancelled)
                                 (incf handler-calls)
                                 (let ((autolith-supervisor::*parent-web-open-function*
                                         (lambda (url &key method headers content)
                                           (incf http-calls)
                                           (if (equal (gethash "action" arguments) "run")
                                               (progn
                                                 (assert (eq method ':post))
                                                 (assert (equal url "https://trusted.invalid/alpha/search"))
                                                 (assert (assoc "Authorization" headers :test #'equalp))
                                                 (assert (equal (gethash "model" (cl-rfc8628:json-decode content))
                                                                "trusted-model")))
                                               (assert (eq method ':get)))
                                           (when (eq case ':failed)
                                            (error 'tool-error :tool-name "fixture"
                                                               :message   "Private transport details"))
                                           (values
                                            (make-string-input-stream
                                             (cond
                                               ((equal (gethash "action" arguments) "run")
                                                "{\"output\":\"serialized web result\"}")
                                               ((eq case ':large-gist)
                                                (make-string 70000 :initial-element #\a))
                                               (t
                                                "page result")))
                                            200 (autolith-supervisor::parent-web-test--map
                                                 "content-type" "text/plain")))))
                                   (when (eq case ':denied)
                                     (autolith-supervisor::parent-web--reject ':denied))
                                   (autolith-supervisor::parent-web-operation
                                    application arguments
                                    (if (eq case ':cancelled) (constantly t) cancelled))))))))
                     (store       (autolith-supervisor::session-store-open path "client-session"))
                     (runtime     (autolith-supervisor::session-runtime-open
                                   store parent :features '("session.v1" "application.v1")
                                   :application-service   service)))
                (unwind-protect
                     (progn
                       (autolith-supervisor::session-runtime-start-turn runtime "web-turn" "Find a page")
                       (agent-session-test--until-finish runtime)
                       (agent-session-test--stop runtime))
                  (autolith-supervisor::session-runtime-disconnect runtime)
                  (autolith-supervisor::session-runtime-await-close runtime :timeout 5))))
            (lambda (client)
              (agent-protocol-handshake client :features '("session.v1" "application.v1"))
              (let ((session
                      (agent-session-test--start
                       client
                       (lambda (session observer)
                         (declare (ignore observer))
                         (handler-bind ((error (lambda (condition) (setf problem condition))))
                          (let* ((configuration (make-configuration :web-search-mode "disabled"))
                                 (*tool-result-overflow-function*
                                   (lambda (text)
                                     (setf retained text)
                                     "context:large-gist-fixture"))
                                 (context       (make-instance 'tool-context :configuration configuration :worker nil
                                                              :conversation (agent-protocol-session-conversation session)))
                                 (gist-p        (member case '(:gist :large-gist)))
                                 (tool          (if gist-p
                                                    (make-instance 'web-gist-tool :namespace "web_extra" :name "gist"
                                                                                 :description "Fixture" :parameters (web-gist-parameters))
                                                    (make-instance 'web-run-tool :namespace "web" :name "run"
                                                                                :description "Fixture" :parameters (web-run-parameters))))
                                 (arguments     (if gist-p (json-object "url" "https://page.invalid/")
                                                    (json-object "search_query" (json-array (json-object "q" "fixture"))))))
                           (if (member case '(:denied :failed :cancelled :forged-scope))
                               (let ((*agent-application-scope-id* (when (eq case ':forged-scope) "not-admitted")))
                                 (assert (handler-case (progn (tool-execute tool context arguments) nil)
                                           (tool-error (condition)
                                             (assert (not (search "Private transport" (princ-to-string condition))))
                                             t))))
                               (let ((result (tool-execute tool context arguments)))
                                 (assert (tool-result-success-p result))
                                 (if (eq case ':large-gist)
                                     (assert (= 70000 (length retained)))
                                     (assert (search (if gist-p "page result" "serialized web result")
                                                     (tool-result-content result))))))
                           (setf ran t)))))))
                (unwind-protect
                     (progn
                       (agent-protocol-session-join session :timeout 8)
                       (assert (agent-protocol-session-stopped session))
                       (assert (null (agent-protocol-session-failure session))))
                  (agent-protocol-session-close session)
                  (agent-protocol-session-join session :timeout 3)))))
        (agent-session-test--cleanup path)))
    (assert ran () "Web case ~A failed: ~A" case problem)
    (assert (= handler-calls (if (eq case ':forged-scope) 0 1)))
    (assert (= http-calls (if (member case '(:denied :cancelled :forged-scope)) 0 1))))
  nil)

(-> run-agent-protocol-web-tests () null)
(defun run-agent-protocol-web-tests ()
  "Check web tool routing, refusal, cancellation, and scope authority without live HTTP."
  (dolist (case '(:run :gist :large-gist :denied :failed :cancelled :forged-scope))
    (agent-web-test--roundtrip case))
  nil)
