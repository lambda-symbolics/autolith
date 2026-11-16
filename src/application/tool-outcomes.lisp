(in-package #:autolith)

;;;; -- Fullscreen Tool Outcomes --

(-> application--tool-outcome-key (application keyword t) (option list))
(defun application--tool-outcome-key (application kind identifier)
  "Scope a provider call or job IDENTIFIER to APPLICATION's conversation."
  (when (non-empty-string-p identifier)
    (list kind
          (conversation-identifier (application-conversation application))
          identifier)))

(-> application--entry-body (list) list)
(defun application--entry-body (entry)
  "Return ENTRY from its first body newline, preserving styled widgets."
  (loop for tail on entry
        for span = (first tail)
        for text = (terminal-span-text span)
        for newline = (position #\Newline text)
        when newline
          return (if (zerop newline)
                     tail
                     (cons (if (termdown:widget-p span)
                               (termdown:make-widget (terminal-span-style span)
                                                    (subseq text newline)
                                                    (termdown:widget-action span))
                               (terminal-span (terminal-span-style span)
                                              (subseq text newline)))
                           (rest tail)))))

(-> application--tool-outcome-body (application list) list)
(defun application--tool-outcome-body (application rows)
  "Indent styled outcome ROWS beneath the original invocation."
  (loop for row in rows
        append (cons (terminal-span ':plain (string #\Newline))
                     (application--tool-row-spans application row))))

(-> application--tool-result-outcome
    (application list list &key (:metadata-only-p boolean)) (option list))
(defun application--tool-result-outcome (application record entry &key metadata-only-p)
  "Describe RECORD's outcome, omitting formatted body work when METADATA-ONLY-P."
  (let* ((properties (rest record))
         (call-key   (application--tool-outcome-key
                      application ':call (getf properties :call-id)))
         (handoff    (application--tool-execution-handoff record))
         (job        (getf (rest handoff) :job))
         (state      (cond
                       (handoff
                        ':pending)
                       ((eq (getf properties :status) ':error)
                        ':failure)
                       (t
                        ':success))))
    (when call-key
      (list :kind            ':result
            :call-key        call-key
            :job-key         (application--tool-outcome-key application ':job (getf job :id))
            :state           state
            :retain-output-p (not (null entry))
            :entry-transform (when (and (eq state ':failure)
                                        (equal (getf properties :tool) "resource.edit"))
                               #'application--resource-edit-failure-entry)
            :body            (unless metadata-only-p
                               (cond
                                 (handoff
                                  (cons (terminal-span ':plain (string #\Newline)) entry))
                                 ((and (eq state ':success) entry
                                       (typep (application--find-tool application (getf properties :tool))
                                              'papercut-report-tool))
                                  ;; Prominent notices, such as papercut reports, retain their heading.
                                  (cons (terminal-span ':plain (string #\Newline)) entry))
                                 (t
                                  (application--entry-body entry))))))))

(-> application--job-completion-outcome
    (application list list &key (:metadata-only-p boolean)) (option list))
(defun application--job-completion-outcome (application properties entry &key metadata-only-p)
  "Associate a job notice with its invocation, optionally omitting formatted body work."
  (let* ((completion (getf properties :job-completion))
         (outcome    (getf completion :outcome))
         (job-key    (application--tool-outcome-key
                      application ':job (getf completion :job-id)))
         (state      (case outcome
                       (:success
                        ':success)
                       ((:failure :cancelled :aborted)
                        ':failure)
                       (otherwise
                        ':pending)))
         (summary    (getf completion :summary)))
    (when job-key
      (list :kind ':completion :job-key job-key :state state
            :retain-output-p (or (not (eq state ':success))
                                 (not (application-compact-view-p application)))
            :body            (unless metadata-only-p
                               (cond
                                 ((and (eq state ':failure) (non-empty-string-p summary))
                                  (application--tool-outcome-body
                                   application
                                   (application--failure-result-rows summary)))
                                 ((eq state ':pending)
                                  (cons (terminal-span ':plain (string #\Newline)) entry))
                                 ((not (application-compact-view-p application))
                                  (application--entry-body entry))))))))

(-> application--record-tool-outcome-basic
    (application list list &key (:item t) (:metadata-only-p boolean)) (option list))
(defun application--record-tool-outcome-basic
    (application record entry &key (item nil item-supplied-p) metadata-only-p)
  "Extract RECORD's identity and lifecycle state, reusing decoded provider ITEM.

METADATA-ONLY-P suppresses formatted outcome bodies during history selection."
  (case (first record)
    (:provider-item
     (let* ((wire-json (getf (rest record) :wire-json))
            (item      (if item-supplied-p
                           item
                           (and (stringp wire-json) (application--transcript-json wire-json))))
            (call-key  (when (and (json-object-p item)
                                  (equal (json-get item "type") "function_call"))
                         (application--tool-outcome-key
                          application ':call (json-get item "call_id")))))
       (when call-key
         (list :kind ':call :call-key call-key :state ':pending
               :expandable-input-p (equal (function-call-canonical-name item) "lisp.eval")))))
    (:tool-result
     (application--tool-result-outcome application record entry :metadata-only-p metadata-only-p))
    (:message
     (when (application--job-completion-message-p (rest record))
       (application--job-completion-outcome application (rest record) entry
                                            :metadata-only-p metadata-only-p)))))

(-> application--tool-outcome-origin (application list) (option list))
(defun application--tool-outcome-origin (application metadata)
  "Recover an evicted invocation before METADATA's sequence, newest records first."
  (let ((call-key (getf metadata :call-key))
        (job-key  (getf metadata :job-key))
        (before   (getf metadata :seq)))
    (when (or call-key job-key)
      (block found
        (conversation-map-records-reverse
         (application-conversation application)
         (lambda (record)
           (let ((sequence (application--record-sequence record)))
             (when (or (null before) (null sequence) (< sequence before))
               (cond
                 ((and (null call-key) (eq (first record) ':tool-result))
                  (let* ((handoff (application--tool-execution-handoff record))
                         (job     (getf (rest handoff) :job)))
                    (when (equal job-key (application--tool-outcome-key
                                          application ':job (getf job :id)))
                      (setf call-key (application--tool-outcome-key
                                      application ':call (getf (rest record) :call-id))))))
                 ((and call-key (eq (first record) ':provider-item))
                  (let ((candidate (application--record-tool-outcome-basic application record nil)))
                    (when (equal call-key (getf candidate :call-key))
                      (return-from found
                        (application--record-presentation application record))))))))))
        nil))))

(-> application--record-tool-outcome (application list list) (option list))
(defun application--record-tool-outcome (application record entry)
  "Retain the logical outcome source alongside its current fullscreen metadata."
  (let ((metadata (application--record-tool-outcome-basic application record entry)))
    (when (and entry (getf metadata :retain-output-p)
               (terminal-ui-fullscreen-p (application-ui application)))
      (let ((layout (application--record-layout application record)))
        (setf (getf metadata :body-source-characters) (application--record-source-weight record)
              (getf metadata :body-layout)
              (lambda (columns)
                (let ((entry (funcall layout columns)))
                  (getf (application--record-tool-outcome-basic application record entry) :body))))))
    (when (and (member (getf metadata :kind) '(:result :completion))
               (terminal-ui-fullscreen-p (application-ui application)))
      (let ((keys (list :call-key (getf metadata :call-key)
                        :job-key  (getf metadata :job-key)
                        :seq      (application--record-sequence record))))
        (setf (getf metadata :origin-function)
              (lambda () (application--tool-outcome-origin application keys)))))
    metadata))
