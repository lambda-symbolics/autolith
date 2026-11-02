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

(-> application--tool-result-outcome (application list list) (option list))
(defun application--tool-result-outcome (application record entry)
  "Describe RECORD's semantic outcome without interpreting its output text."
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
            :body            (cond
                               (handoff
                                (cons (terminal-span ':plain (string #\Newline)) entry))
                               ((and (eq state ':success) entry
                                     (not (eq (terminal-span-style (first entry)) ':success)))
                                ;; Prominent notices, such as papercut reports, retain their heading.
                                (cons (terminal-span ':plain (string #\Newline)) entry))
                               (t
                                (application--entry-body entry)))))))

(-> application--job-completion-outcome (application list list) (option list))
(defun application--job-completion-outcome (application properties entry)
  "Associate a durable job notice with its handed-off invocation."
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
            :body            (cond
                               ((and (eq state ':failure) (non-empty-string-p summary))
                                (application--tool-outcome-body
                                 application
                                 (application--failure-result-rows summary)))
                               ((eq state ':pending)
                                (cons (terminal-span ':plain (string #\Newline)) entry))
                               ((not (application-compact-view-p application))
                                (application--entry-body entry)))))))

(-> application--record-tool-outcome (application list list) (option list))
(defun application--record-tool-outcome (application record entry)
  "Extract invocation identity and lifecycle metadata for a fullscreen RECORD."
  (case (first record)
    (:provider-item
     (let* ((wire-json (getf (rest record) :wire-json))
            (item      (and (stringp wire-json) (json-decode wire-json)))
            (call-key  (when (and (json-object-p item)
                                  (equal (json-get item "type") "function_call"))
                         (application--tool-outcome-key
                          application ':call (json-get item "call_id")))))
       (when call-key
         (list :kind ':call :call-key call-key :state ':pending))))
    (:tool-result
     (application--tool-result-outcome application record entry))
    (:message
     (when (application--job-completion-message-p (rest record))
       (application--job-completion-outcome application (rest record) entry)))))
