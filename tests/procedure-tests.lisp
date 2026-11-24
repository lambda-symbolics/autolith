(in-package #:autolith)

;;;; -- Procedures --

(-> test-procedure-definition () null)
(defun test-procedure-definition ()
  "Test that a defined procedure calls tools as functions and that a retired one does not run."
  (with-test-configuration (base root)
    (let* ((configuration (configuration-copy base :working-directory root))
           (registry      (make-default-tool-registry :configuration configuration))
           (context       (make-instance 'tool-context
                                         :configuration configuration :worker nil :registry registry
                                         :conversation (conversation-create configuration))))
      (test-write-text (merge-pathnames "notes.txt" root) "quarterly notes")
      (flet ((call (name &rest arguments)
               (test-resource-call registry context
                                   :namespace "procedure" :name name :arguments arguments)))
        (test-assert (not (tool-result-success-p
                           (call "define" "id" "read-note" "source" "(lambda (name) \"Read NAME.\" name)")))
                     "a procedure with positional parameters is rejected")
        (test-assert (tool-result-success-p
                      (call "define" "id" "read-note"
                            "source" "(lambda (&key name)
  \"Read the workspace file NAME.\"
  (resource.read :uri (format nil \"workspace:~A\" name)))"))
                     "a procedure is defined")
        (test-assert (search "quarterly notes"
                             (tool-result-content
                              (call "run" "id" "read-note" "arguments" (json-object "name" "notes.txt"))))
                     "the procedure calls a tool as a function")
        (procedure-retire configuration "read-note")
        (test-assert (not (tool-result-success-p
                           (call "run" "id" "read-note" "arguments" (json-object "name" "notes.txt"))))
                     "a retired procedure does not run"))))
  nil)
