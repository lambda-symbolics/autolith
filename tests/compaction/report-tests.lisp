(in-package #:autolith)

;;;; -- Compaction Failure Reports --

(-> compaction-report-test--directory (configuration) pathname)
(defun compaction-report-test--directory (configuration)
  "Return the absolute report directory for CONFIGURATION."
  (merge-pathnames "compaction-reports/"
                   (merge-pathnames (config :state-root configuration) (uiop:getcwd))))

(-> compaction-report-test--files (configuration) list)
(defun compaction-report-test--files (configuration)
  "Return CONFIGURATION's saved reports without creating their directory."
  (directory (merge-pathnames "*.txt" (compaction-report-test--directory configuration))))

(-> compaction-report-test--escaping (function) (option serious-condition))
(defun compaction-report-test--escaping (function)
  "Return FUNCTION's escaping serious condition, or NIL after normal completion."
  (handler-case (progn (funcall function) nil)
    (serious-condition (condition)
      condition)))

(-> compaction-report-test--saved-path (configuration list) pathname)
(defun compaction-report-test--saved-path (configuration details)
  "Check the path-only notification contract and return the saved pathname."
  (test-assert (and (= 2 (length details))
                    (eq :path (first details))
                    (stringp (second details)))
               "A successful report notification contains only its path.")
  (let ((path (pathname (getf details :path))))
    (test-assert (eq :absolute (first (pathname-directory path)))
                 "A report notification identifies an absolute pathname.")
    (test-assert (equal (compaction-report-test--directory configuration)
                        (uiop:pathname-directory-pathname path))
                 "The report is saved below the configured state root.")
    (test-assert (probe-file path) "The announced report exists.")
    path))

(-> test-compaction-report-save () null)
(defun test-compaction-report-save ()
  "Save distinct reports and recover their complete UTF-8 contents."
  (with-test-configuration (configuration)
    (let* ((text   (format nil "Příliš žluťoučký λ~%Second diagnostic line.~%"))
           (first  (compaction-report-save configuration text))
           (second (compaction-report-save configuration text)))
      (test-assert (not (equal first second)) "Repeated saves allocate different files.")
      (test-assert (= 2 (length (compaction-report-test--files configuration)))
                   "Both reports survive the second save.")
      (dolist (path (list first second))
        (compaction-report-test--saved-path configuration (list :path (namestring path)))
        (test-assert (string= text (uiop:read-file-string path :external-format ':utf-8))
                     "Report serialization preserves UTF-8 text exactly."))))
  nil)

(-> test-compaction-report-agent-failures () null)
(defun test-compaction-report-agent-failures ()
  "Report real refusal, final projection admission and pre-write publication failures."
  (dolist (stage '(:refusal :projection :write))
    (compaction-owner-test--fixture
     (lambda (application provider)
       (let* ((configuration (application-configuration application))
              (conversation  (application-conversation application))
              (events        nil)
              (original      nil)
              (arrival-hit-p nil)
              (note-original (symbol-function 'agent--note-compaction-request))
              (write-hit-p   nil)
              (write-record  (symbol-function 'conversation--write-rotated-record))
              (write-failure (make-condition 'simple-error :format-control "Fixture checkpoint write failure."))
              (expected-type (ecase stage
                               (:refusal
                                'provider-protocol-error)
                               (:projection
                                'provider-context-budget-error)
                               (:write
                                'conversation-invariant-error)))
              (observer
                (make-instance 'callback-agent-observer
                               :status-callback (lambda (status details)
                                                  (push (list status details) events)))))
         (setf (compaction-owner-results provider)
               (list (agent-test-result
                      "report-response"
                      (list (if (eq stage ':refusal)
                                (agent-test-refusal)
                                (agent-test-checkpoint-call))))))
         (let ((escaped
                 (compaction-report-test--escaping
                  (lambda ()
                    (handler-bind
                        ((error (lambda (condition)
                                  (when (and (null original) (typep condition expected-type))
                                    (setf original condition)))))
                      (compaction-owner-test--call-with-credentials
                       application
                       (lambda ()
                         (test-call-with-function-replacements
                          (case stage
                            (:projection
                             (list
                              (list 'agent--note-compaction-request
                                    (lambda (owner observer &rest arguments)
                                      (multiple-value-prog1 (apply note-original owner observer arguments)
                                        (unless arrival-hit-p
                                          (setf arrival-hit-p t)
                                          (conversation-append-user-message
                                           conversation (make-string 600000 :initial-element #\n))))))))
                            (:write
                             (list
                              (list 'conversation--write-rotated-record
                                    (lambda (owner record)
                                      (if (eq :summary (first record))
                                          (progn (setf write-hit-p t) (error write-failure))
                                          (funcall write-record owner record)))))))
                          (lambda ()
                            (agent-compact-conversation (application-agent application) observer))))))))))
           (test-assert (and original (eq original escaped) (typep escaped expected-type))
                        (format nil "The original ~S failure escapes the real compaction boundary." stage))
           (when (eq stage ':projection)
             (test-assert arrival-hit-p "The oversized canonical USER arrival follows the paid summary.")
             (when (typep escaped 'provider-context-budget-error)
               (compaction-owner-test--refusal-budget escaped)))
           (when (eq stage ':write)
             (test-assert write-hit-p "The real checkpoint append reaches its write boundary."))
           (test-assert (equal '(t) (compaction-owner-compaction-flags provider))
                        "Exactly one summary request reaches the inert production transport.")
           (let ((failures (remove-if-not (lambda (event) (eq :compaction-failed (first event))) events)))
             (test-assert (= 1 (length failures)) "An aborted checkpoint announces one report.")
             (when (= 1 (length failures))
               (let* ((path (compaction-report-test--saved-path configuration (second (first failures))))
                      (text (uiop:read-file-string path :external-format ':utf-8)))
                 (test-assert (search "report-response" text)
                              "The report retains the completed parsed response snapshot."))))
           (test-assert (= 1 (length (compaction-report-test--files configuration)))
                        "The escaping failure saves exactly one report.")
           (dolist (current (list conversation (conversation-load (conversation-pathname conversation))))
             (test-assert (null (conversation-continuation current))
                          "An aborted checkpoint installs no live or reloaded continuation.")
             (test-assert (not (find :summary (conversation-records-newest current 30) :key #'first))
                          "Neither the live nor reloaded conversation gains a summary.")))))))
  nil)

(-> test-compaction-report-committed-listener-failure () null)
(defun test-compaction-report-committed-listener-failure ()
  "Keep a durable summary distinct from a subsequent record-listener failure."
  (compaction-owner-test--fixture
   (lambda (application provider)
     (let* ((configuration (application-configuration application))
            (conversation  (application-conversation application))
            (listener      (conversation-record-listener conversation))
            (original      (make-condition 'simple-error :format-control "Fixture summary listener failure."))
            (summary       nil)
            (events        nil)
            (observer      (make-instance 'callback-agent-observer
                                          :status-callback (lambda (status details)
                                                             (push (list status details) events)))))
       (unwind-protect
            (progn
              (setf (conversation-record-listener conversation)
                    (lambda (record)
                      (when (eq :summary (first record))
                        (setf summary record)
                        (error original))))
              (test-assert
               (eq original
                   (compaction-report-test--escaping
                    (lambda ()
                      (compaction-owner-test--call-with-credentials
                       application
                       (lambda () (agent-compact-conversation (application-agent application) observer))))))
               "A post-commit listener failure escapes without replacement."))
         (setf (conversation-record-listener conversation) listener))
       (test-assert summary "The listener fails specifically on the committed summary.")
       (test-assert (equal '(t) (compaction-owner-compaction-flags provider))
                    "The summary is obtained through production streaming.")
       (let* ((reloaded (conversation-load (conversation-pathname conversation)))
              (record   (find :summary (conversation-records-newest reloaded 30) :key #'first)))
         (test-assert (and record summary
                           (= (getf (rest record) :seq) (getf (rest summary) :seq))
                           (conversation-continuation reloaded))
                      "Reload recovers the exact summary committed before the listener failed."))
       (test-assert (null (compaction-report-test--files configuration))
                    "A committed checkpoint produces no failure report.")
       (test-assert (not (find :compaction-failed events :key #'first))
                    "A post-commit listener failure sends no failed-checkpoint notification."))))
  nil)

(-> test-compaction-report-lifecycle () null)
(defun test-compaction-report-lifecycle ()
  "Preserve values, live recovery restarts and cancellation without false reports."
  (with-test-configuration (configuration)
    (dolist (mode '(:success :recovered :cancelled :published))
      (let* ((original      (if (eq mode ':cancelled)
                                (make-condition 'cl-jobpond:job-aborted
                                                :identifier "compaction-report-fixture"
                                                :reason ':cancelled :message "Fixture cancellation.")
                                (make-condition 'simple-error :format-control "Recoverable fixture failure.")))
             (notifications nil)
             (returned      nil)
             (escaped
               (compaction-report-test--escaping
                (lambda ()
                  (handler-bind
                      ((error (lambda (condition)
                                (when (and (eq mode ':recovered) (eq original condition))
                                  (invoke-restart 'resume-compaction)))))
                    (setf returned
                          (multiple-value-list
                           (compaction-report-call
                            configuration
                            (lambda (context)
                              (ecase mode
                                (:success
                                 (values ':done 17 nil))
                                (:recovered
                                 (restart-case (error original)
                                   (resume-compaction () (values ':done 17 nil))))
                                (:cancelled
                                 (error original))
                                (:published
                                 (setf (compaction-report-published-p context) t)
                                 (error original))))
                            :notify (lambda (details) (push details notifications))))))))))
        (if (member mode '(:success :recovered))
            (test-assert (and (null escaped) (equal '(:done 17 nil) returned))
                         "Normal completion and recovery preserve all values, including trailing NIL.")
            (test-assert (eq original escaped) "Cancellation and later adapter errors preserve condition identity."))
        (test-assert (null notifications) (format nil "~S sends no failure notification." mode))
        (test-assert (null (compaction-report-test--files configuration))
                     (format nil "~S saves no failed-checkpoint report." mode)))))
  nil)

(-> test-compaction-report-secondary-failures () null)
(defun test-compaction-report-secondary-failures ()
  "Keep filesystem and notification errors secondary to the original failure."
  (dolist (stage '(:save :notify))
    (with-test-configuration (configuration)
      (let* ((original      (make-condition 'simple-error :format-control "Original compaction failure."))
             (secondary     (make-condition 'simple-error :format-control "Diagnostic adapter failure."))
             (blocker       (merge-pathnames "compaction-reports" (config :state-root configuration)))
             (notifications nil)
             (escaped       nil))
        (when (eq stage ':save)
          (ensure-directories-exist blocker)
          (with-open-file (stream blocker :direction ':output :if-exists ':error
                                          :if-does-not-exist ':create :external-format ':utf-8)
            (write-string "Fixture file blocks report-directory creation." stream)))
        (setf escaped
              (compaction-report-test--escaping
               (lambda ()
                 (compaction-report-call
                  configuration (lambda (context) (declare (ignore context)) (error original))
                  :notify (lambda (details)
                            (push details notifications)
                            (when (eq stage ':notify) (error secondary)))))))
        (test-assert (eq original escaped) "A secondary failure cannot replace the compaction condition.")
        (test-assert (= 1 (length notifications)) "The reporting adapter is called once.")
        (let ((details (first notifications)))
          (ecase stage
            (:save
             (test-assert (and (= 2 (length details))
                               (eq :save-error (first details))
                               (stringp (second details)))
                          "A real report-directory creation failure exposes only its save-error diagnostic."))
            (:notify
             (compaction-report-test--saved-path configuration details)
             (test-assert (= 1 (length (compaction-report-test--files configuration)))
                          "Notification failure does not discard the saved report.")))))))
  nil)

(-> test-compaction-report-signal-time-redaction () null)
(defun test-compaction-report-signal-time-redaction ()
  "Redact condition text and nested provider bodies before credential ownership unwinds."
  (with-test-configuration (configuration)
    (let* ((secrets  (oauth-credentials-secret-values (provider-tests--credentials configuration)))
           (message  (format nil "Příliš λ: ~{~A~^ ~}" secrets))
           (response (json-object "public" "PUBLIC-RESPONSE-FIELD"
                                  "nested" (json-array
                                            (json-object "opaque_echo" (first secrets)
                                                        "deep" (json-object "echoes" (coerce secrets 'vector)))))))
      (dolist (original (list (make-condition 'simple-error :format-control "~A" :format-arguments (list message))
                             (make-condition 'provider-protocol-error
                                             :message message :status nil :code "report-fixture"
                                             :request-id "public-request" :response-id "public-response"
                                             :response response)))
        (let* ((*provider-active-credential-values* nil)
               (notifications                       nil)
               (notified-outside-p                  nil)
               (escaped
                 (compaction-report-test--escaping
                  (lambda ()
                    (compaction-report-call
                     configuration
                     (lambda (context)
                       (declare (ignore context))
                       (let ((*provider-active-credential-values*           secrets)
                             (*provider-active-credential-redaction-marker* *provider-credential-redaction-marker*))
                         (error original)))
                     :notify (lambda (details)
                               (setf notified-outside-p (null *provider-active-credential-values*))
                               (push details notifications)))))))
          (test-assert (eq original escaped) "Redaction preserves the original condition object.")
          (test-assert (and notified-outside-p (null *provider-active-credential-values*))
                       "The report is saved and announced after request credentials unwind.")
          (test-assert (= 1 (length notifications)) "Each credential-scoped error saves one report.")
          (let* ((details (first notifications))
                 (path    (compaction-report-test--saved-path configuration details))
                 (text    (uiop:read-file-string path :external-format ':utf-8)))
            (provider-tests--assert-credential-free (list text details) secrets
                                                   "Condition text, nested provider bodies and notifications contain no credentials.")
            (test-assert (and (search "Příliš λ" text) (search *provider-credential-redaction-marker* text))
                         "The UTF-8 condition diagnostic survives with an explicit redaction marker.")
            (when (typep original 'provider-error)
              (test-assert (search "PUBLIC-RESPONSE-FIELD" text)
                           "The sanitized nested provider response retains useful nonsensitive data.")))))))
  nil)

(-> compaction-report-test--echo-sse (list) string)
(defun compaction-report-test--echo-sse (secrets)
  "Encode inert credential echoes, including tokens split across text deltas."
  (with-output-to-string (stream)
    (flet ((emit (object)
             "Append one standard SSE event to the inert response."
             (write-string (test-sse-event-string object) stream)))
      (emit (json-object "type" "response.created"
                         "response" (json-object "id" (first secrets))))
      (dolist (secret secrets)
        (let ((split (floor (length secret) 2)))
          (dolist (part (list (subseq secret 0 split) (subseq secret split)))
            (emit (json-object "type" "response.output_text.delta" "delta" part)))))
      (emit (json-object "type" "response.output_item.done"
                         "item" (agent-test-message (format nil "Public snapshot λ ~{~A~^ ~}" secrets))))
      (emit (json-object "type" "response.completed"
                         "response" (json-object "id" (first secrets)
                                                 "usage" (json-object "input_tokens" 100 "output_tokens" 50)))))))

(-> test-compaction-report-production-result-redaction () null)
(defun test-compaction-report-production-result-redaction ()
  "Capture a production parsed result inside credentials, then report a later failure safely."
  (compaction-owner-test--fixture
   (lambda (application provider)
     (let* ((configuration                       (application-configuration application))
            (secrets                             (oauth-credentials-secret-values (provider-tests--credentials configuration)))
            (original                            (make-condition 'simple-error :format-control "Failure after credential ownership."))
            (*provider-active-credential-values* nil)
            (notifications                       nil)
            (events                              nil)
            (captured-p                          nil)
            (inside-p                            nil)
            (outside-p                           nil)
            (escaped                             nil))
       (flet ((attempt (context)
                "Perform the real provider attempt, then fail outside its credential ownership."
                (declare (ignore context))
                (let* ((capture *provider-result-diagnostic-function*)
                       (*provider-result-diagnostic-function*
                         (lambda (result)
                           (setf captured-p t
                                 inside-p (every (lambda (secret)
                                                   (member secret *provider-active-credential-values* :test #'string=))
                                                 secrets))
                           ;; Test the report callback independently of wire redaction.
                           (setf (gethash "diagnostic_echo" (provider-result-usage result))
                                 (json-object "public_probe" "CAPTURED-NESTED-SNAPSHOT"
                                              "nested" (json-object "opaque_echo" (format nil "~{~A~^ ~}" secrets))))
                           (funcall capture result)))
                       (returned
                         (provider-attempt-turn
                          provider (application-conversation application)
                          :tool-namespaces (continuation-tool-namespaces)
                          :compaction-p    t
                          :event-callback  (lambda (event) (push event events)))))
                  (setf outside-p (null *provider-active-credential-values*)
                        (gethash "public_probe" (gethash "diagnostic_echo" (provider-result-usage returned)))
                        "MUTABLE-NESTED-MARKER"
                        (gethash "after_capture" (provider-result-usage returned)) "MUTABLE-RESULT-MARKER")
                  (error original))))
         (setf escaped
               (compaction-report-test--escaping
                (lambda ()
                  (compaction-owner-test--call-with-credentials
                   application
                   (lambda ()
                     (test-call-with-function-replacements
                      (list (list 'compaction-owner-test--sse
                                  (lambda (result)
                                    (declare (ignore result))
                                    (compaction-report-test--echo-sse secrets))))
                      (lambda ()
                        (compaction-report-call
                         configuration #'attempt
                         :notify (lambda (details) (push details notifications))))))))))
         (test-assert (eq original escaped) "The later error escapes without replacement.")
         (test-assert (and captured-p inside-p outside-p)
                      "The real provider callback runs inside credentials and the later error runs outside them.")
         (test-assert (find-if (lambda (event) (typep event 'assistant-delta-event)) events)
                      "Split credential deltas pass through the production decoder.")
         (test-assert (= 1 (length notifications)) "The later failure announces one report.")
         (let* ((details (first notifications))
                (path    (compaction-report-test--saved-path configuration details))
                (text    (uiop:read-file-string path :external-format ':utf-8)))
           (provider-tests--assert-credential-free (list text details) secrets
                                                  "The saved production snapshot contains no echoed credentials.")
           (test-assert (and (search "Public snapshot" text)
                             (search "diagnostic_echo" text)
                             (search *provider-credential-redaction-marker* text))
                        "The report includes useful sanitized output and callback-captured extensions.")
           (test-assert (and (search "CAPTURED-NESTED-SNAPSHOT" text)
                             (not (search "MUTABLE-NESTED-MARKER" text))
                             (not (search "MUTABLE-RESULT-MARKER" text)))
                        "The saved response is deeply detached from later parsed-result mutations."))))))
  nil)

(-> test-compaction-report-headless-announcement () null)
(defun test-compaction-report-headless-announcement ()
  "Announce the saved report path through real application presentation with no UI."
  (compaction-owner-test--fixture
   (lambda (application provider)
     (declare (ignore provider))
     (let* ((ui                                           (application-ui application))
            (path                                         (namestring (compaction-report-save (application-configuration application) "Fixture report λ")))
            (*application-command-presentation-capture-p* t)
            (*application-command-presentation-pending-p* nil)
            (*application-command-presentation-text*      nil))
       (unwind-protect
            (progn
              (setf (application-ui application) nil)
              (application-present-compaction-report application (list :path path))
              (test-assert (find-if (lambda (text) (search path text)) *application-command-presentation-text*)
                           "Headless application-present includes the saved path."))
         (setf (application-ui application) ui)))))
  nil)
