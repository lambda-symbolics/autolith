(in-package #:autolith)

;;;; -- Shell Interpreter Discipline --

(defparameter *interpreter-discipline-command-fragments*
    '("python3 -c" "python -c")
  "Shell command fragments identifying an inline interpreter one-liner.")

(defparameter *interpreter-discipline-instruction*
    "A shell.run call in this logical turn executed an inline interpreter one-liner such as python3 -c. This violates tool policy: route ad hoc computation and text transformation through lisp.eval in a disposable SBCL worker instead of an external interpreter. Unless the user requested Python, the workspace is a Python project, or a required dependency makes Python appropriate, redo comparable work with lisp.eval and do not run further interpreter one-liners."
  "The corrective advice injected after a shell interpreter one-liner.")

(-> interpreter-discipline--one-liner-call-p (t) boolean)
(defun interpreter-discipline--one-liner-call-p (item)
  "Return true when ITEM is a shell.run call with an interpreter one-liner."
  (and (json-object-p item)
       (string= (or (json-get item "type") "") "function_call")
       (string= (or (json-get item "namespace") "") "shell")
       (string= (or (json-get item "name") "") "run")
       (let ((arguments (json-get item "arguments")))
         (and (stringp arguments)
              (loop for fragment in *interpreter-discipline-command-fragments*
                      thereis (and (search fragment arguments) t))))))

(-> interpreter-discipline--turn-offense-p (conversation) boolean)
(defun interpreter-discipline--turn-offense-p (conversation)
  "Return true when the current logical turn ran an interpreter one-liner."
  (block nil
    (dolist (item (reverse (conversation-input-items conversation)) nil)
      (when (and (json-object-p item)
                 (eq (item-role item) ':user))
        (return nil))
      (when (interpreter-discipline--one-liner-call-p item)
        (return t)))))

(-> interpreter-discipline-context
    (request-context)
    (option context-contribution))
(defun interpreter-discipline-context (request)
  "Rebuke an interpreter one-liner run during the current logical turn."
  (when (and (not (request-context-compaction-p request))
             (interpreter-discipline--turn-offense-p
              (request-context-conversation request)))
    (make-context-contribution
     :identifier        "interpreter-discipline"
     :instruction       *interpreter-discipline-instruction*
     :priority          40
     :lifetime          ':turn
     :class             ':mandatory
     :deduplication-key "interpreter-discipline")))

(register-context-contributor "interpreter-discipline"
                              'interpreter-discipline-context
                              :source ':built-in)
