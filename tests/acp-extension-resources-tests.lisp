(in-package #:autolith)

;;;; -- Structured Resource Extension Tests --

(-> acp-extension-resource-test--reject (function symbol) null)
(defun acp-extension-resource-test--reject (function condition-type)
  "Require FUNCTION to signal the requested typed failure."
  (test-assert (handler-case (progn (funcall function) nil)
                 (error (condition) (typep condition condition-type)))
               "Resource request signals its expected typed failure")
  nil)

(-> test-acp-extension-resource-windows () null)
(defun test-acp-extension-resource-windows ()
  "Exercise real revisions, bounded defaults, line offsets and oversized requests."
  (with-test-configuration (configuration root)
    (let ((path (merge-pathnames "known-resource.txt" root))
          (uri "workspace:known-resource.txt"))
      (with-open-file (stream path :direction ':output :if-exists ':supersede)
        (dotimes (index 450) (format stream "line-~D~%" (1+ index))))
      (acp-session-test--call-with-client
       configuration
       (lambda (service client)
         (let* ((identifier (agentcomms:client-new-session client (namestring root)))
                (session (acp-service--session service identifier))
                (one (acp-extension-resource
                      session (agentcomms:json-object "uri" uri "startLine" 2 "lineCount" 1)))
                (default (acp-extension-resource session (agentcomms:json-object "uri" uri)))
                (window (gethash "window" default)))
           (test-assert (equal "line-2" (gethash "content" one)) "Exact nonempty line content")
           (test-assert (equal (gethash "revision" one) (gethash "revision" default))
                        "Two unchanged observations share their real revision")
           (test-assert (and (stringp (gethash "revision" one))
                             (= 400 (gethash "lineCount" window))
                             (= 450 (gethash "totalLines" window))
                             (= 401 (gethash "nextLine" window)))
                        "Default line window is bounded with a continuation")
           (with-open-file (stream path :direction ':output :if-exists ':append)
             (write-line "changed" stream))
           (test-assert
            (not (equal (gethash "revision" one)
                        (gethash "revision" (acp-extension-resource
                                             session (agentcomms:json-object "uri" uri)))))
            "Content mutation changes the resource revision")
           (dolist (arguments (list (agentcomms:json-object "uri" uri "startLine" 0)
                                    (agentcomms:json-object "uri" uri "lineCount" 1001)
                                    (agentcomms:json-object "uri" uri "byteCount" 1)))
             (acp-extension-resource-test--reject
              (lambda () (acp-extension-resource session arguments)) 'agentcomms:acp-method-error))
           (acp-extension-resource-test--reject
            (lambda () (acp-extension-resource session (agentcomms:json-object "uri" "workspace:.")))
            'acp-extension-unavailable)
           (with-open-file (stream path :direction ':output :if-exists ':supersede)
             (write-string (make-string 65537 :initial-element #\x) stream))
           (acp-extension-resource-test--reject
            (lambda () (acp-extension-resource session (agentcomms:json-object "uri" uri)))
            'acp-extension-unavailable))))))
  nil)

(-> test-acp-extension-resource-authorization () null)
(defun test-acp-extension-resource-authorization ()
  "Check owned raw log reads, byte defaults, end offsets and foreign ownership."
  (with-test-configuration (configuration root)
    (acp-session-test--call-with-client
     configuration
     (lambda (service client)
       (let* ((identifier (agentcomms:client-new-session client (namestring root)))
              (session (acp-service--session service identifier))
              (context (acp-extension-tool-context session))
              (artifact (shell-log-allocate context))
              (path (merge-pathnames "output.bin" (shell-log-capture-directory artifact)))
              (bytes (utf8-string-to-octets
                      (concatenate 'string "abcdef" (make-string 5000 :initial-element #\x))))
              (capture (make-instance 'cl-exec-sandbox:sandbox-capture :path path))
              (uri (shell-log-reference artifact ':output)))
         (with-open-file (stream path :direction ':output :if-exists ':supersede
                                      :element-type '(unsigned-byte 8))
           (write-sequence bytes stream))
         (setf (cl-exec-sandbox:sandbox-capture-byte-count capture) (length bytes)
               (cl-exec-sandbox:sandbox-capture-observed-byte-count capture) (length bytes)
               (cl-exec-sandbox:sandbox-capture-complete-p capture) t
               (cl-exec-sandbox:sandbox-capture-status capture) ':complete)
         (shell-log-record-capture
          artifact (make-instance 'cl-exec-sandbox:sandbox-result
                                  :output-capture capture :exit-code 0
                                  :timed-out-p nil :cancelled-p nil :status ':exited))
         (let* ((result (acp-extension-resource
                         session (agentcomms:json-object "uri" uri "byteOffset" 2 "byteCount" 3)))
                (window (gethash "window" result))
                (default (acp-extension-resource session (agentcomms:json-object "uri" uri)))
                (end (acp-extension-resource
                      session (agentcomms:json-object "uri" uri "byteOffset" 99999))))
           (test-assert (equal "cde" (gethash "content" result)) "Owned raw bytes are content")
           (test-assert (and (= 2 (gethash "byteOffset" window))
                             (= 3 (gethash "byteCount" window))
                             (= 5 (gethash "nextByteOffset" window))
                             (= 5006 (gethash "totalBytes" window))) "Raw byte window positions")
           (test-assert (= 4096 (length (gethash "content" default))) "Default raw window cap")
           (test-assert (and (equal "" (gethash "content" end))
                             (= 5006 (gethash "byteOffset" (gethash "window" end))))
                        "Past-end offsets return an empty window at EOF")
           (test-assert (eq ':null (gethash "revision" result)) "Appendable logs have no fabricated revision"))
         (dolist (arguments (list (agentcomms:json-object "uri" uri "byteCount" 4097)
                                  (agentcomms:json-object "uri" uri "byteOffset" -1)
                                  (agentcomms:json-object "uri" uri "startLine" 1)))
           (acp-extension-resource-test--reject
            (lambda () (acp-extension-resource session arguments)) 'agentcomms:acp-method-error))
         (acp-extension-resource-test--reject
          (lambda () (acp-extension-resource
                      session (agentcomms:json-object "uri" "shell-log:foreign/execution/artifact/output")))
          'resource-access-denied)
         (acp-extension-resource-test--reject
          (lambda () (acp-extension-resource session (agentcomms:json-object "uri" "memory:all")))
          'acp-extension-unavailable)))))
  nil)
