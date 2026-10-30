(in-package #:autolith)

;;;; -- Relax Warning Tests --

(-> relax-tests--application (configuration) application)
(defun relax-tests--application (configuration)
  "Return a local application with shared provider configuration and private history."
  (make-instance 'application :configuration configuration
                 :conversation               (conversation-create configuration)
                 :ui                         (terminal-ui-create
                                              :terminal (make-instance 'stream-terminal
                                                                       :input-stream  (make-string-input-stream "")
                                                                       :output-stream (make-string-output-stream)
                                                                       :input-file-descriptor -1 :columns 80))))

(-> relax-tests--marker-count (string string) (integer 0))
(defun relax-tests--marker-count (text marker)
  "Count non-overlapping occurrences of MARKER in TEXT."
  (loop with start = 0
        with count = 0
        for position = (search marker text :start2 start)
        while position
        do (incf count)
           (setf start (+ position (length marker)))
        finally (return count)))

(-> test-relax-setting-and-command () null)
(defun test-relax-setting-and-command ()
  "Test native and slash relax controls without creating a conversation turn."
  (let* ((configuration        (test-configuration))
         (application          (relax-tests--application configuration))
         (conversation-bound-p (slot-boundp application 'conversation)))
    (unwind-protect
         (progn
           (test-assert (eq (application-run-lisp-input application "(relax t)") ':continue)
                        "native relax input is dispatched as a command")
           (test-assert (application-relax-p application)
                        "native relax input enables the session setting")
           (test-assert (eq (application-apply-setting application :relax-p nil) nil)
                        "settings application accepts the relax setting")
           (test-assert (not (application-relax-p application))
                        "settings application disables the relax setting")
           (application-command application "/relax on" :interactive-p nil)
           (test-assert (application-relax-p application)
                        "slash relax on enables the session setting")
           (application-command application "/relax off" :interactive-p nil)
           (test-assert (not (application-relax-p application))
                        "slash relax off disables the session setting")
           (test-assert (eq conversation-bound-p (slot-boundp application 'conversation))
                        "relax controls do not create a conversation")
           (when (slot-boundp application 'conversation)
             (test-assert (null (conversation-input-items (application-conversation application)))
                          "relax controls do not append a user or assistant item")))
      (platform-delete-directory-tree *platform* (test-configuration-root configuration)
                                      :validate t :if-does-not-exist ':ignore))))

(-> test-relax-warning-rides-session-state () null)
(defun test-relax-warning-rides-session-state ()
  "Deliver the private reminder behind the history, keeping the cached instructions."
  (with-test-configuration (configuration)
    (let* ((provider                      (provider-create configuration))
           (conversation                  (conversation-create configuration))
           (*system-prompt-relax-warning* "RELAX-TEST-MARKER-7e2d")
           (marker                        *system-prompt-relax-warning*))
      (flet ((instructions ()
               (json-get (provider-request-object provider conversation #()) "instructions"))

             (request-text ()
               (json-encode (provider-request-object provider conversation #()))))
        (let ((disabled-instructions (instructions)))
          (test-assert (not (search marker (request-text)))
                       "disabled relax adds no reminder to outgoing requests")
          (setf (config :relax-p configuration) t)
          (test-assert (= 1 (relax-tests--marker-count (request-text) marker))
                       "the enabled reminder reaches the provider request exactly once")
          (test-assert (string= disabled-instructions (instructions))
                       "toggling relax leaves the cached instructions unchanged"))
        (conversation-append-user-message conversation "earlier request")
        (conversation-append-summary conversation "compacted earlier request")
        (test-assert (= 1 (relax-tests--marker-count (request-text) marker))
                     "the next outgoing request includes the reminder after compaction")
        (test-assert (not (test-object-contains-string-p
                           (conversation-input-items conversation) marker))
                     "the reminder is excluded from durable conversation history")
        (let ((*system-prompt-override* "A bounded helper task."))
          (test-assert (= 1 (relax-tests--marker-count (system-prompt configuration) marker))
                       "overridden system prompts also include the enabled reminder"))
        (setf (config :relax-p configuration) nil)
        (test-assert (not (search marker (request-text)))
                     "disabling relax affects the next outgoing provider request"))))
  nil)
