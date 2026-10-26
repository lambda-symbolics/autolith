;;;; -- Supervisor Package --

(defpackage #:autolith-supervisor
  (:use #:cl)
  (:documentation "The standalone process supervisor, independent of agent code and configuration.")
  (:export #:run-agent
           #:main
           #:invalid-launch-argument
           #:invalid-launch-argument-name
           #:invalid-launch-argument-value
           #:agent-process-error
           #:agent-process-error-command
           #:agent-process-error-cause))
