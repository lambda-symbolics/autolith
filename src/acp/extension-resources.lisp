(in-package #:autolith)

;;;; -- Bounded ACP Resource Views --

(-> acp-extension-resource--integer
    (hash-table string &key (:default integer) (:minimum integer) (:maximum integer)) integer)
(defun acp-extension-resource--integer (params name &key default minimum maximum)
  "Read a bounded window parameter, using DEFAULT only when omitted."
  (let ((value (gethash name params default)))
    (unless (and (integerp value) (<= minimum value maximum))
      (agentcomms:acp-invalid-params
       "Parameter ~A must be an integer between ~D and ~D." name minimum maximum))
    value))

(-> acp-extension-resource--read (resource tool-context hash-table) hash-table)
(defgeneric acp-extension-resource--read (resource context params)
  (:documentation "Project an authorized resource as bounded content and exact window metadata.")
  (:method ((resource resource) (context tool-context) params)
    "Reject resources without a structured projection."
    (declare (ignore resource context params))
    (error 'acp-extension-unavailable :reason "Resource has no structured ACP projection.")))

(defmethod acp-extension-resource--read
    ((resource workspace-file-resource) (context tool-context) params)
  "Project one authoritative snapshot without a second read or prose parsing."
  (when (or (nth-value 1 (gethash "byteOffset" params))
            (nth-value 1 (gethash "byteCount" params)))
    (agentcomms:acp-invalid-params "Workspace resources require line windows."))
  (let* ((start (acp-extension-resource--integer
                 params "startLine" :default 1 :minimum 1 :maximum most-positive-fixnum))
         (count (acp-extension-resource--integer
                 params "lineCount" :default 400 :minimum 1 :maximum 1000))
         (observation (resource-observe resource context))
         (lines (workspace-file-observation-lines observation))
         (total (length lines))
         (first (min total (1- start)))
         (last first)
         (characters 0))
    (unless (eq (workspace-file-observation-kind observation) ':file)
      (error 'acp-extension-unavailable :reason "ACP workspace reads require a regular text file."))
    ;; Whole lines preserve paging semantics. Reject a single oversized line
    ;; rather than returning unbounded content or silently omitting text.
    (loop while (< last (min total (+ first count)))
          for size = (+ (length (aref lines last)) (if (= last first) 0 1))
          while (<= (+ characters size) 65536)
          do (incf characters size) (incf last))
    (when (and (= first last) (< first total))
      (error 'acp-extension-unavailable :reason "Resource line exceeds the 65536-character window limit."))
    (agentcomms:json-object
     "uri" (resource-uri resource)
     "revision" (resource-observation-revision observation)
     "encoding" "utf-8" "mime" "text/plain"
     "content" (format nil "~{~A~^~%~}" (coerce (subseq lines first last) 'list))
     "window" (agentcomms:json-object
               "kind" "lines" "startLine" (1+ first) "lineCount" (- last first)
               "totalLines" total "nextLine" (if (< last total) (1+ last) ':null)
               "truncated" (if (< last total) t (argo:json-false))))))

(defmethod acp-extension-resource--read
    ((resource shell-log-resource) (context tool-context) params)
  "Read raw owned capture bytes without parsing a rendered tool result."
  (when (or (nth-value 1 (gethash "startLine" params))
            (nth-value 1 (gethash "lineCount" params)))
    (agentcomms:acp-invalid-params "Shell logs require byte windows."))
  (let* ((offset (acp-extension-resource--integer
                  params "byteOffset" :default 0 :minimum 0 :maximum most-positive-fixnum))
         (count (acp-extension-resource--integer
                 params "byteCount" :default 4096 :minimum 1 :maximum 4096))
         (metadata (shell-log-resource-metadata resource))
         (capture (shell-log-resource--capture resource))
         (file (getf capture :file)))
    (unless (shell-log--authorized-p metadata context)
      (error 'resource-access-denied :uri (resource-uri resource) :operation ':read))
    (when (or (member (getf metadata :state) '(:missing :pruned)) (null file))
      (error 'acp-extension-unavailable :reason "Shell log capture is unavailable."))
    (unless (shell-log--capture-file-p file)
      (shell-log--fail "Invalid shell capture filename in manifest."))
    (multiple-value-bind (bytes total next)
        (shell-log-resource--read-bytes
         (tool-context-configuration context)
         (merge-pathnames file (shell-log-resource-directory resource))
         :offset offset :count count)
      (agentcomms:json-object
       "uri" (resource-uri resource) "revision" ':null
       "encoding" "utf-8" "mime" "text/plain"
       "content" (cl-exec-sandbox:decode-capture-bytes bytes)
       "window" (agentcomms:json-object
                 "kind" "bytes" "byteOffset" (min offset total)
                 "byteCount" (length bytes) "totalBytes" total
                 "nextByteOffset" (if (< next total) next ':null)
                 "truncated" (if (< next total) t (argo:json-false)))))))

(-> acp-extension-resource (acp-session hash-table) hash-table)
(defun acp-extension-resource (session params)
  "Return bounded structured content after ordinary resource ownership checks."
  (let ((uri (gethash "uri" params))
        (context (acp-extension-tool-context session)))
    (unless (and (stringp uri) (plusp (length uri)))
      (agentcomms:acp-invalid-params "Resource uri must be a nonempty string."))
    (multiple-value-bind (scheme identifier) (resource-uri-parse uri)
      (declare (ignore identifier))
      (unless (member scheme '("workspace" "shell-log") :test #'string=)
        (error 'acp-extension-unavailable :reason "ACP resource reads support workspace and shell-log URIs.")))
    (acp-extension-resource--read
     (resource-registry-resolve
      (tool-registry-resource-registry (tool-context-registry context)) uri context)
     context params)))
