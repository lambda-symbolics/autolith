(in-package #:autolith)

(-> test-task-tool-default-argument-types () null)
(defun test-task-tool-default-argument-types ()
  "Test that explicit JSON false and null never become omitted task defaults."
  (let* ((configuration (test-configuration))
         (root          (test-configuration-root configuration))
         (registry      (task-augment-tool-registry (make-default-tool-registry)))
         (primary
          (task-tests--primary-agent configuration "task-default-types" registry))
         (conversation  (agent-conversation primary))
         (context
          (make-instance 'tool-context :configuration configuration :worker nil
                         :conversation conversation :registry registry :agent primary)))
    (unwind-protect
         (labels ((rejected-p (namespace name arguments)
                    "Return true when actual registry dispatch rejects ARGUMENTS."
                    (not
                     (tool-result-success-p
                      (tool-registry-execute-call registry
                                                  (json-object "namespace" namespace
                                                               "name" name "arguments"
                                                               arguments)
                                                  context)))))
           (dolist
               (case
                '(("job" "list" "{\"offset\":false}") ("job" "list" "{\"offset\":null}")
                  ("job" "list" "{\"limit\":false}") ("job" "list" "{\"limit\":null}")))
             (destructuring-bind
                 (namespace name arguments)
                 case
               (test-assert (rejected-p namespace name arguments)
                            (format nil
                                    "~A.~A rejects explicit non-integer default input ~A"
                                     namespace name arguments)))))
      (ignore-errors (tool-registry-close-runtime-state registry))
      (platform-delete-directory-tree *platform* root :validate t :if-does-not-exist ':ignore)))
  nil)
