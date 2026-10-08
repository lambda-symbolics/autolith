(in-package #:autolith)

;;;; -- Shell Preview and Capture Integration --

(-> shell-output-tests--context (configuration tool-registry) tool-context)
(defun shell-output-tests--context (configuration registry)
  "Return an authorized direct execution context with a durable conversation."
  (make-instance 'tool-context
                 :configuration configuration :worker nil :registry registry
                 :conversation (conversation-create configuration)
                 :command-authorization-function
                 (lambda (command directory)
                   (declare (ignore command directory))
                   ':full-access)))

(-> shell-output-tests--capture-path (tool-context string) pathname)
(defun shell-output-tests--capture-path (context reference)
  "Locate a test capture through its authorized resource and private manifest."
  (let* ((resource (resource-resolver-resolve
                    (make-instance 'shell-log-resolver :scheme "shell-log")
                    (subseq reference 10) context))
         (capture (shell-log-resource--capture resource)))
    (merge-pathnames (getf capture :file) (shell-log-resource-directory resource))))

(-> shell-output-tests--read-bytes (pathname) vector)
(defun shell-output-tests--read-bytes (path)
  "Read a test-sized raw capture for exact comparison."
  (with-open-file (stream path :element-type '(unsigned-byte 8))
    (let ((bytes (make-array (file-length stream) :element-type '(unsigned-byte 8))))
      (read-sequence bytes stream)
      bytes)))

(-> shell-output-tests--emit-command (pathname &key (:fail-p boolean)) string)
(defun shell-output-tests--emit-command (path &key fail-p)
  "Return the host command emitting PATH as raw stdout and a trailing stderr diagnostic."
  (let ((quoted (test-fixture-shell-quote *platform* (namestring path))))
    (test-fixture-shell-command
     *platform*
     (format nil "cat ~A; printf 'FINAL-DIAGNOSTIC\\n' >&2; exit ~D" quoted (if fail-p 7 0))
     (format nil "$bytes=[IO.File]::ReadAllBytes(~A); $stdout=[Console]::OpenStandardOutput(); $stdout.Write($bytes,0,$bytes.Length); $stdout.Flush(); [Console]::Error.Write('FINAL-DIAGNOSTIC'+[char]10); exit ~D"
             quoted (if fail-p 7 0)))))

(-> test-shell-retained-preview-and-ranges () null)
(defun test-shell-retained-preview-and-ranges ()
  "Retain exact large merged/separate bytes, head/tail diagnostics and middle range/search."
  (with-test-configuration (configuration root)
    (let* ((registry (make-default-tool-registry))
           (context (shell-output-tests--context configuration registry))
           (tool (tool-registry-find registry "shell" "run"))
           (read-tool (tool-registry-find registry "resource" "read"))
           (payload-path (merge-pathnames "raw-output.bin" root))
           (text (concatenate 'string "BEGIN-OUTPUT\n" (make-string 40000 :initial-element #\x)
                              "MIDDLE-NEEDLE" (make-string 40000 :initial-element #\y)
                              "END-OUTPUT\n"))
           (payload (utf8-string-to-octets text))
           (overflow-count 0)
           (*tool-result-overflow-function*
             (lambda (content)
               (declare (ignore content))
               (incf overflow-count)
               "context:unexpected")))
      (setf (aref payload 10) 255)
      (with-open-file (stream payload-path :direction ':output
                                          :element-type '(unsigned-byte 8))
        (write-sequence payload stream))
      (dolist (separate-p '(nil t))
        (let* ((result (tool-execute
                        tool context
                        (json-object "command" (shell-output-tests--emit-command payload-path :fail-p t)
                                   "separate-output" separate-p)))
               (details (tool-result-details result))
               (metadata (first (getf details :shell-logs)))
               (captures (getf metadata :captures))
               (capture (find ':output captures :key (lambda (entry) (getf entry :stream))))
               (reference (getf capture :reference))
               (actual (shell-output-tests--read-bytes
                         (shell-output-tests--capture-path context reference)))
               (expected (if separate-p payload
                             (concatenate '(vector (unsigned-byte 8)) payload
                                          (utf8-string-to-octets (format nil "FINAL-DIAGNOSTIC~%"))))))
          (test-assert (and (tool-result-success-p result) (= 7 (getf details :exit-code)))
                       "Nonzero exit status is preserved independently of execution success")
          (test-assert (equalp expected actual) "Retained raw capture is byte-exact, including invalid UTF-8")
          (test-assert (and (getf capture :complete-p)
                            (= (length actual) (getf capture :byte-count)))
                       "Complete metadata names the actual retained byte size")
          (test-assert (and (search "BEGIN-OUT" (tool-result-content result))
                            (search "END-OUTPUT" (tool-result-content result))
                            (search "FINAL-DIAGNOSTIC" (tool-result-content result))
                            (< (length (tool-result-content result)) 8000))
                       "Bounded preview includes beginning and trailing diagnostics")
          (test-assert (= (length captures) (if separate-p 2 1))
                       "Separate captures preserve distinct stream identities")
          (let ((range (tool-execute read-tool context
                                    (json-object "uri" reference "byte-offset" 39990 "byte-count" 100)))
                (search-result (tool-execute read-tool context
                                            (json-object "uri" reference "query" "MIDDLE-NEEDLE"))))
            (test-assert (search "MIDDLE-NEEDLE" (tool-result-content range))
                         "Authorized middle range is readable")
            (test-assert (search "MIDDLE-NEEDLE" (tool-result-content search-result))
                         "Authorized search retrieves an omitted middle diagnostic"))))
      (test-assert (zerop overflow-count) "Raw logs and previews never spill into context storage")))
  nil)

(-> test-shell-prelaunch-failure () null)
(defun test-shell-prelaunch-failure ()
  "Report the original launch failure when no sandbox capture result exists."
  (with-test-configuration (configuration root)
    (declare (ignore root))
    (let* ((registry (make-default-tool-registry))
           (context (shell-output-tests--context configuration registry))
           (result
             (test-call-with-function-replacements
              (list (list 'run-sandboxed
                          (lambda (&rest arguments)
                            (declare (ignore arguments))
                            (error 'cl-exec-sandbox:sandbox-execution-error
                                   :message "sandbox-launch-probe"
                                   :command '("fixture")))))
              (lambda ()
                (tool-execute (tool-registry-find registry "shell" "run") context
                              (json-object "command" "fixture"))))))
      (test-assert (not (tool-result-success-p result))
                   "A prelaunch failure reports unsuccessful execution")
      (test-assert (search "sandbox-launch-probe" (tool-result-content result))
                   "The launch diagnostic is preserved without a capture result")))
  nil)

(-> test-shell-retained-timeout () null)
(defun test-shell-retained-timeout ()
  "Retain pre-timeout output with truthful incomplete status."
  (with-test-configuration (configuration root)
    (declare (ignore root))
    (let* ((registry (make-default-tool-registry))
           (context (shell-output-tests--context configuration registry))
           (result (tool-execute
                    (tool-registry-find registry "shell" "run") context
                    (json-object
                     "command" (test-fixture-shell-command
                                *platform* "printf BEFORE-TIMEOUT; sleep 5"
                                "[Console]::Write('BEFORE-TIMEOUT'); Start-Sleep -Seconds 5")
                     "timeout-seconds" 1)))
           (metadata (first (getf (tool-result-details result) :shell-logs)))
           (capture (first (getf metadata :captures))))
      (test-assert (not (tool-result-success-p result)) "Timeout reports a failed execution")
      (test-assert (and (getf metadata :timed-out-p)
                        (not (getf capture :complete-p))
                        (plusp (getf capture :byte-count)))
                   "Timeout retains bytes and never claims a complete capture")
      (test-assert (search "BEFORE-TIMEOUT" (tool-result-content result))
                   "Timeout preview includes captured diagnostics")))
  nil)


(-> test-shell-log-export-boundary () null)
(defun test-shell-log-export-boundary ()
  "Collect task assets without reading private command-output subtrees."
  (with-test-configuration (configuration root)
    (declare (ignore root))
    (let* ((owner "log-export-owner")
           (asset-root (data-transfer--asset-root configuration ':tasks owner))
           (terminal (merge-pathnames "execution/terminal.sexp" asset-root))
           (capture (merge-pathnames "execution/shell-log/artifact/raw.bin" asset-root)))
      (ensure-directories-exist capture)
      (dolist (path (list terminal capture))
        (with-open-file (stream path :direction ':output)
          (write-string "asset" stream)))
      (let* ((original (symbol-function 'data-transfer--bytes))
             (assets
               (test-call-with-function-replacements
                (list (list 'data-transfer--bytes
                            (lambda (path)
                              (test-assert (not (equal path capture))
                                           "Session export never reads retained log bytes")
                              (funcall original path))))
                (lambda () (data-transfer--assets configuration ':tasks owner)))))
        (test-assert (and (= 1 (length assets))
                          (equal (getf (first assets) :path) '("execution" "terminal.sexp")))
                     "Session task metadata is collected independently of log storage"))))
  nil)
