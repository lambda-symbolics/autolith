(in-package #:autolith)

;;;; -- Model Command Permission Classification --

(defparameter *permissions-model-decision-guidance*
  autolith-supervisor::*permissions-model-decision-guidance*
  "Shared task guiding one model command permission decision.")

(defparameter *permissions-model-decision-contract*
  '(:type :object
    :properties
    (("decision" (:type :string :enum ("sandboxed" "full" "deny")))
     ("reason" (:type :string)))
    :required ("decision" "reason")
    :additional-properties nil)
  "The output contract one model permission decision must satisfy.")

(defparameter *permissions-model-call-budget* 2
  "The provider calls one command classification frame may spend.")

(defparameter *permissions-model-token-budget* 8000
  "The tokens one command classification frame may spend.")

(-> application--command-sandbox-available-p () boolean)
(defun application--command-sandbox-available-p ()
  "Return whether the workspace command sandbox can enforce network isolation."
  (handler-case
      (and (sandbox-supported-p ':network-isolated) t)
    (error ()
      nil)))

(-> application--command-sandbox-unavailable-message () string)
(defun application--command-sandbox-unavailable-message ()
  "Return an actionable platform-specific command sandbox diagnostic."
  (if (string-equal (software-type) "Linux")
      "The workspace command sandbox is unavailable. On Linux, install Bubblewrap (the bwrap executable) to provide filesystem and network isolation. Until it is available, sandbox mode is disabled and command approval choices run with full user privileges."
      "The workspace command sandbox is unavailable. On Windows, rebuild or reinstall the native cl-exec-sandbox helper. Sandbox mode is disabled and command approval choices run with full user privileges."))

(-> permissions--model-decision-keyword (t) (option keyword))
(defun permissions--model-decision-keyword (decision)
  "Return the automatic permission keyword DECISION names, or NIL when unknown."
  (cond
    ((equal decision "sandboxed")
     ':sandboxed)
    ((equal decision "full")
     ':full-access)
    ((equal decision "deny")
     ':deny)
    (t
     nil)))

(-> permissions-model-classify-command
    (string pathname
     &key (:provider model-provider)
          (:configuration configuration)
          (:sandbox-available-p boolean)
          (:user-instructions (option string)))
    (values keyword string))
(defun permissions-model-classify-command
    (command directory
     &key provider configuration (sandbox-available-p t) user-instructions)
  "Classify COMMAND in DIRECTORY with one bounded inference frame.

Return :SANDBOXED, :FULL-ACCESS, or :DENY plus the model's reason.
USER-INSTRUCTIONS supplies the current user's authorization-relevant request.
Every unavailable grant, failure, malformed answer, or exhausted budget denies."
  (handler-case
      (let* ((*telemetry-request-role* ':permission-classifier)
             (value
               (infer *permissions-model-decision-guidance*
                      :context
                      (list (list ':label "current user instructions"
                                  ':content
                                  (if (non-empty-string-p user-instructions)
                                      user-instructions
                                      "unavailable; do not infer user approval"))
                            (list ':label "command"
                                  ':content command)
                            (list ':label "working directory"
                                  ':content (namestring directory))
                            (list ':label "workspace sandbox"
                                  ':content
                                  (if sandbox-available-p
                                      "available"
                                      "unavailable; never choose sandboxed")))
                      :contract      *permissions-model-decision-contract*
                      :budget        (rlm-budget-create
                                      :calls  *permissions-model-call-budget*
                                      :tokens *permissions-model-token-budget*
                                      :depth  0)
                      :effort        "low"
                      :provider      provider
                      :configuration configuration))
             (pairs                    (rest value))
             (decision                 (permissions--model-decision-keyword
                                        (second (assoc "decision" pairs :test #'string=))))
             (reason                   (second (assoc "reason" pairs :test #'string=))))
        (cond
          ((and (eq decision ':sandboxed) (not sandbox-available-p))
           (values ':deny "the workspace sandbox is unavailable"))
          (decision
           (values decision
                   (if (non-empty-string-p reason)
                       reason
                       "the model gave no reason")))
          (t
           (values ':deny "the model returned an unknown decision"))))
    (error (condition)
      (declare (ignore condition))
      (values ':deny "the model classifier failed, so the command was denied"))))
