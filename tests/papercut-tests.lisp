(in-package #:autolith)

;;;; -- Papercut Tests --

(-> papercut-tests--call
    (tool-registry tool-context string &rest t)
    tool-result)
(defun papercut-tests--call (registry context canonical-name &rest arguments)
  "Execute CANONICAL-NAME through REGISTRY and CONTEXT."
  (let ((separator (position #\. canonical-name)))
    (unless separator
      (error "A test tool name must contain a namespace: ~A" canonical-name))
    (tool-registry-execute-call
     registry
     (json-object
      "namespace" (subseq canonical-name 0 separator)
      "name" (subseq canonical-name (1+ separator))
      "arguments" (json-encode (apply #'json-object arguments)))
     context)))

(-> papercut-tests--error-p (function) boolean)
(defun papercut-tests--error-p (function)
  "Return true when FUNCTION signals PAPERCUT-ERROR."
  (handler-case
      (progn
        (funcall function)
        nil)
    (papercut-error ()
      t)))

(-> test-papercut-assessment-state () null)
(defun test-papercut-assessment-state ()
  "Test append-only papercut assessment replay, validation, and closure policy."
  (let* ((configuration (test-configuration))
         (root (test-configuration-root configuration)))
    (unwind-protect
         (let* ((report
                  (papercut-report
                   configuration
                   :title "Assessment target"
                   :content "Measure whether the intervention helped."))
                (identifier (papercut-identifier report)))
           (test-assert
            (and (null (papercut-assessment-verdict report))
                 (null (papercut-assessment-note report))
                 (null (papercut-assessed-at report)))
            "new and legacy papercuts begin unassessed")
           (dolist (verdict '(:worse :unchanged :too-early))
             (let* ((note (format nil "Latest outcome is ~A." verdict))
                    (assessed
                      (papercut-assess configuration identifier
                                       :verdict verdict
                                       :note note))
                    (replayed (papercut-find configuration identifier)))
               (test-assert
                (and (eq (papercut-assessment-verdict assessed) verdict)
                     (string= (papercut-assessment-note assessed) note)
                     (typep (papercut-assessed-at assessed) 'timestamp)
                     (eq (papercut-assessment-verdict replayed) verdict)
                     (string= (papercut-assessment-note replayed) note))
                "assessment replay retains the latest verdict, note, and timestamp")
               (test-assert
                (papercut-tests--error-p
                 (lambda ()
                   (papercut-mark-closed
                    configuration identifier
                    :resolution "This outcome does not permit closure.")))
                "worse, unchanged, and too-early assessments prevent closure")))
           (test-assert
            (papercut-tests--error-p
             (lambda ()
               (papercut-assess configuration identifier
                                :verdict ':unknown
                                :note "Invalid verdict.")))
            "papercut assessments reject unknown verdicts")
           (test-assert
            (papercut-tests--error-p
             (lambda ()
               (papercut-assess
                configuration identifier
                :verdict ':improved
                :note (make-string (1+ *papercut-assessment-note-limit*)
                                   :initial-element #\x))))
            "papercut assessment notes have a hard size bound")
           (papercut-assess configuration identifier
                            :verdict ':improved
                            :note "The intervention now works reliably.")
           (test-assert
            (eq (papercut-assessment-verdict
                 (papercut-find configuration identifier))
                ':improved)
            "a newer improved assessment supersedes an unfavorable verdict")
           (papercut-mark-closed
            configuration identifier
            :resolution "Latest assessment confirms the fix.")
           (test-assert (null (papercut-find configuration identifier))
                        "improved papercuts may be closed")
           (test-assert
            (papercut-tests--error-p
             (lambda ()
               (papercut-assess configuration identifier
                                :verdict ':improved
                                :note "Closed reports cannot be reassessed.")))
            "closed papercuts cannot be assessed")
           (let ((legacy
                   (papercut-report
                    configuration
                    :title "Legacy closure"
                    :content "No assessment was recorded.")))
             (papercut-mark-closed
              configuration
              (papercut-identifier legacy)
              :resolution "Legacy closure remains valid.")
             (test-assert
              (null (papercut-find configuration (papercut-identifier legacy)))
              "unassessed legacy papercuts may be closed")))
      (uiop:delete-directory-tree root :validate t :if-does-not-exist ':ignore)))
  (let* ((configuration (test-configuration))
         (root (test-configuration-root configuration)))
    (unwind-protect
         (let ((report
                 (papercut-report
                  configuration
                  :title "Invalid persisted closure"
                  :content "Replay must enforce assessment closure policy.")))
           (with-lock-held (*papercut-lock*)
             (sexp-store:log-append
              (configuration-papercut-path configuration)
              (papercut--assessed-record
               (papercut-identifier report) ':worse "It regressed."
               (get-universal-time)))
             (sexp-store:log-append
              (configuration-papercut-path configuration)
              (papercut--closed-record
               (papercut-identifier report) "Invalid closure."
               (get-universal-time))))
           (test-assert
            (papercut-tests--error-p
             (lambda () (papercut-list configuration)))
            "replay rejects closures following unfavorable assessments"))
      (uiop:delete-directory-tree root :validate t :if-does-not-exist ':ignore)))
  nil)

(-> test-papercuts () null)
(defun test-papercuts ()
  "Test papercut persistence, model reporting, presentation, and commands."
  (test-papercut-assessment-state)
  (let* ((configuration (test-configuration))
         (root (test-configuration-root configuration))
         (registry (make-default-tool-registry))
         (conversation (conversation-create configuration
                                            :identifier "papercut-tool"))
         (context (make-instance 'tool-context
                                 :configuration configuration
                                 :worker nil
                                 :conversation conversation
                                 :registry registry))
         (terminal (make-instance 'recording-terminal :columns 80))
         (ui (terminal-ui-create :terminal terminal))
         (application (make-instance 'application
                                     :configuration configuration
                                     :conversation conversation
                                     :tool-registry registry
                                     :ui ui)))
    (unwind-protect
         (progn
           (let* ((first
                    (papercut-report
                     configuration
                     :title "Provider retry is opaque"
                     :content "The provider failed twice without a useful diagnostic."
                     :source-conversation "first"))
                  (second
                    (papercut-report
                     configuration
                     :title "Recovery image cannot start"
                     :content "The recovery image exits before it can accept a request."
                     :source-conversation "second")))
             (test-assert (= (length (papercut-list configuration)) 2)
                          "papercut reports persist in the current workspace")
             (test-assert (papercut-find configuration (papercut-identifier first))
                          "papercut reports can be found by their full identifier")
             (multiple-value-bind (resolved status matches)
                 (papercut-resolve
                  configuration
                  (papercut-short-identifier first))
               (test-assert (and (eq status ':found)
                                 (null matches)
                                 resolved
                                 (string= (papercut-identifier resolved)
                                          (papercut-identifier first)))
                            "unique papercut identifier prefixes resolve exactly"))
             (test-assert
              (handler-case
                  (progn
                    (papercut-report
                     configuration
                     :title ""
                     :content "This report must be rejected.")
                    nil)
                (papercut-error ()
                  t))
              "papercut titles reject empty strings")
             (test-assert
              (handler-case
                  (progn
                    (papercut-report
                     configuration
                     :title "Oversized"
                     :content (make-string (1+ *papercut-content-limit*)
                                           :initial-element #\x))
                    nil)
                (papercut-error ()
                  t))
              "papercut bodies have a hard size bound")
             (test-assert (papercut-find configuration (papercut-identifier second))
                          "multiple papercut reports retain distinct identifiers")
             (let ((closed
                     (papercut-report
                      configuration
                      :title "Obsolete report"
                      :content "This report is ready to close.")))
               (papercut-mark-closed
                configuration
                (papercut-identifier closed)
                :resolution "The underlying problem was fixed.")
               (test-assert
                (and (= (length (papercut-list configuration)) 2)
                     (null (papercut-find
                            configuration (papercut-identifier closed))))
                "papercut closure tombstones remove reports from active state")
               (test-assert
                (handler-case
                    (progn
                      (papercut-mark-closed
                       configuration
                       (papercut-identifier closed)
                       :resolution "Duplicate closure must fail.")
                      nil)
                  (papercut-error ()
                    t))
                "papercuts cannot be closed more than once")))
           (let* ((result
                    (papercut-tests--call
                     registry
                     context
                     "papercut.report"
                     "title" "Provider limit"
                     "content" "The provider returned a rate limit twice."))
                  (report
                    (find "Provider limit"
                          (papercut-list configuration)
                          :test #'string=
                          :key #'papercut-title))
                  (tool (tool-registry-find registry "papercut" "report")))
             (test-assert (tool-result-success-p result)
                          "papercut.report returns a successful tool result")
             (test-assert (and report
                               (search (papercut-identifier report)
                                       (tool-result-content result))
                               (not (search (papercut-content report)
                                            (tool-result-content result))))
                          "papercut.report acknowledges with an identifier without echoing the body")
             (test-assert (and tool
                               (eq (tool-conversation-persistence tool)
                                   ':next-response)
                               (tool-compact-result-visible-p tool))
                          "papercut reports are request-local and remain visible in compact mode")
             (let ((conversation-path (conversation-pathname conversation)))
               (when (probe-file conversation-path)
                 (delete-file conversation-path))
               (conversation-append-provider-item
                conversation
                (json-object
                 "type" "function_call"
                 "call_id" "papercut-call"
                 "name" "papercut.report"
                 "arguments"
                 (json-encode
                  (json-object "title" "Private title"
                               "content" "Private diagnostic body")))
                :persistence ':next-response)
               (conversation-append-tool-result
                conversation
                "papercut-call"
                :tool-name "papercut.report"
                :output (tool-result-content result)
                :success-p t
                :persistence ':next-response)
               (test-assert (not (probe-file conversation-path))
                            "papercut calls and results stay out of conversation files"))
             (let* ((record
                      (list :tool-result
                            :tool "papercut.report"
                            :status ':ok
                            :output (tool-result-content result)))
                    (entry (application-tool-result-entry tool application record))
                    (text (terminal--spans-text entry)))
               (test-assert
                (and (search "! PAPERCUT RECORDED" text)
                     (search "Provider limit" text)
                     (search "The provider returned a rate limit twice." text)
                     (search (papercut-call-source report) text))
                "successful papercut reports render as prominent complete cards")
               (test-assert
                (and (application--record-visible-p application record)
                     (conversation-record-entry application record))
                "compact transcript filtering retains papercut result cards")))
           (terminal-ui-start ui)
           (recording-terminal-reset terminal)
           (let* ((report
                    (find "Provider limit"
                          (papercut-list configuration)
                          :test #'string=
                          :key #'papercut-title))
                  (output
                    (format nil
                            "papercut-id: ~A~%title: ~A"
                            (papercut-identifier report)
                            (papercut-title report)))
                  (observer (application-agent-observer application))
                  (send-status
                    (callback-agent-observer-status-callback observer)))
             (funcall
              send-status
              ':tool-call-completed
              (list :tool "papercut.report"
                    :success-p t
                    :output output))
             (test-assert
              (search "! PAPERCUT RECORDED"
                      (recording-terminal-output terminal))
              "completed request-local papercuts are presented immediately"))
           (recording-terminal-reset terminal)
           (test-assert (eq (application-command application "/papercuts") ':continue)
                        "/papercuts remains inside the interactive application")
           (let ((list-output (recording-terminal-output terminal)))
             (test-assert
              (and (search "PAPERCUTS" list-output)
                   (search "(papercut \"" list-output)
                   (search "Provider retry is opaque" list-output)
                   (not (search "~%" list-output)))
              "papercut listing shows canonical expansion calls"))
           (recording-terminal-reset terminal)
           (let* ((report (first (papercut-list configuration)))
                  (evaluation
                    (application-lisp-evaluate
                     (papercut-call-source report)
                     :application application)))
             (test-assert
              (eq (application-lisp-evaluation-status evaluation) ':ok)
              "the displayed papercut call evaluates successfully")
             (let ((detail-output (recording-terminal-output terminal)))
               (test-assert
                (and (search "PAPERCUT" detail-output)
                     (search (papercut-title report) detail-output)
                     (search (papercut-content report) detail-output)
                     (not (search "~%" detail-output)))
                "the displayed papercut call expands the complete report body")))
           (recording-terminal-reset terminal)
           (let* ((report (first (papercut-list configuration)))
                  (source
                    (format nil "(papercut-close ~S)"
                            (papercut-short-identifier report)))
                  (evaluation
                    (application-lisp-evaluate source :application application)))
             (test-assert
              (eq (application-lisp-evaluation-status evaluation) ':ok)
              "the canonical papercut-close call evaluates successfully")
             (test-assert
              (and (search "PAPERCUT CLOSED"
                           (recording-terminal-output terminal))
                   (null (papercut-find
                          configuration (papercut-identifier report))))
              "the canonical papercut-close call persists a closure tombstone"))
           (test-assert (search "/papercuts" (application-help))
                        "interactive help includes /papercuts")
           (test-assert (search "/papercut [ID]" (application-help))
                        "interactive help includes optional /papercut syntax")
           (test-assert (search "/papercut-close [ID]" (application-help))
                        "interactive help includes optional /papercut-close syntax"))
      (ignore-errors (terminal-ui-stop ui))
      (uiop:delete-directory-tree root :validate t :if-does-not-exist ':ignore)))
  nil)
