(in-package #:autolith)

;;;; -- Shell Interpreter Discipline Tests --

(-> interpreter-discipline-tests--shell-call (string) json-object)
(defun interpreter-discipline-tests--shell-call (command)
  "Return one durable shell.run function call executing COMMAND."
  (json-object
   "type" "function_call"
   "call_id" (make-identifier)
   "name" "run"
   "namespace" "shell"
   "arguments" (json-encode (json-object "command" command))))

(-> interpreter-discipline-tests--contribution
    (configuration conversation)
    (option context-contribution))
(defun interpreter-discipline-tests--contribution (configuration conversation)
  "Return the interpreter-discipline contribution for CONVERSATION, if active."
  (find "interpreter-discipline"
        (context-delivery-contributions
         (context-resolve-request configuration conversation #()))
        :key  #'context-contribution-identifier
        :test #'string=))

(-> test-interpreter-discipline () null)
(defun test-interpreter-discipline ()
  "Test the corrective advice following a shell interpreter one-liner."
  (let* ((configuration                 (test-configuration))
         (conversation                  (conversation-create configuration
                                                             :identifier "discipline-test"))
         (*context-contributors*        nil)
         (*context-last-deliveries*     (make-hash-table :test #'equal))
         (*context-last-delivery-order* nil))
    (register-context-contributor "interpreter-discipline"
                                  'interpreter-discipline-context
                                  :source ':built-in)
    (conversation-append-user-message conversation "sum the report numbers")
    (test-assert
     (null (interpreter-discipline-tests--contribution configuration conversation))
     "a turn without shell calls draws no interpreter rebuke")
    (conversation-append-provider-item
     conversation
     (interpreter-discipline-tests--shell-call "ls -la"))
    (test-assert
     (null (interpreter-discipline-tests--contribution configuration conversation))
     "an ordinary shell command draws no interpreter rebuke")
    (conversation-append-provider-item
     conversation
     (json-object
      "type" "function_call"
      "call_id" (make-identifier)
      "name" "eval"
      "namespace" "lisp"
      "arguments" (json-encode
                   (json-object "forms" (vector "(print \"python3 -c\")")))))
    (test-assert
     (null (interpreter-discipline-tests--contribution configuration conversation))
     "a non-shell tool call mentioning the fragment draws no rebuke")
    (conversation-append-provider-item
     conversation
     (interpreter-discipline-tests--shell-call
      "python3 -c 'print(40 + 2)'"))
    (test-assert
     (not (null (interpreter-discipline-tests--contribution
                 configuration
                 conversation)))
     "a python3 -c shell call draws the interpreter rebuke")
    (conversation-append-user-message conversation "carry on")
    (test-assert
     (null (interpreter-discipline-tests--contribution configuration conversation))
     "a new logical turn clears the interpreter rebuke")
    (conversation-append-provider-item
     conversation
     (interpreter-discipline-tests--shell-call
      "python -c 'import sys; print(sys.platform)'"))
    (test-assert
     (not (null (interpreter-discipline-tests--contribution
                 configuration
                 conversation)))
     "a python -c shell call draws the interpreter rebuke"))
  nil)
