(in-package #:autolith)

;;;; -- Authorized Byte-Window Shell Log Resources --

(defparameter *shell-log-resource-maximum-bytes* 4096
  "Largest raw byte window rendered by one resource read.")
(defparameter *shell-log-resource-scan-bytes* (* 1024 1024)
  "Maximum raw bytes examined by one paginated search.")
(defparameter *shell-log-resource-maximum-results* 20
  "Maximum bounded excerpts returned by one search.")

(defclass shell-log-resource (resource)
  ((directory :initarg :directory :reader shell-log-resource-directory
              :documentation "Private managed artifact directory.")
   (metadata :initarg :metadata :reader shell-log-resource-metadata
             :documentation "Manifest observed during authorized resolution.")
   (stream :initarg :stream :reader shell-log-resource-stream
           :documentation "The selected output or error stream."))
  (:documentation "A read-only private raw capture with bounded text rendering."))

(defclass shell-log-resolver (resource-resolver) ()
  (:documentation "Resolve opaque shell capture identities under session ownership."))

(-> shell-log--authorized-p (list tool-context) boolean)
(defun shell-log--authorized-p (metadata context)
  "Allow the primary session, or a child inspecting its own execution and descendants.
A descendant cannot inspect its ancestor's or a sibling's captures."
  (let* ((agent (tool-context-agent context))
         (root (if agent (task-parent-root-conversation-identifier agent)
                   (conversation-identifier (tool-context-conversation context))))
         (child-job (and (typep agent 'task-child-agent) (task-child-agent-job agent)))
         (execution (and child-job (session-job-execution-identifier child-job))))
    (and (equal root (getf metadata :root-conversation))
         (or (null execution)
             (equal execution (getf metadata :execution-id))
             (member execution (getf metadata :owner-execution-ids) :test #'equal))
         t)))

(defmethod resource-resolver-child-safe-p ((resolver shell-log-resolver) context)
  "Allow child resolution because each reference is checked against durable ownership."
  (declare (ignore resolver context))
  t)

(defmethod resource-resolver-read-documentation ((resolver shell-log-resolver))
  "Document shell-log: reads."
  (declare (ignore resolver))
  "shell-log: reads execution-owned retained raw output as bounded UTF-8 text: use byte-offset/byte-count for ranges, or query with byte-offset/max-results for paginated literal search. Logs are authorized by session and task ownership; a missing or pruned artifact returns a diagnostic. Reads never load an entire log into conversation or context storage.")

(defmethod resource-resolver-resolve
    ((resolver shell-log-resolver) identifier (context tool-context))
  "Resolve opaque identity segments, refusing traversal and foreign sessions first."
  (let* ((uri (format nil "~A:~A" (resource-resolver-scheme resolver) identifier))
         (parts (uiop:split-string identifier :separator "/"))
         (configuration (tool-context-configuration context)))
    (unless (and (= (length parts) 4) (every #'shell-log--component-p parts)
                 (member (fourth parts) '("output" "error") :test #'string=))
      (error 'resource-access-denied :uri uri :operation ':read))
    (destructuring-bind (root execution artifact stream) parts
      ;; Check the root before inspecting even the existence of a foreign manifest.
      (unless (shell-log--authorized-p (list :root-conversation root
                                            :execution-id execution
                                            :owner-execution-ids (list execution)) context)
        ;; Descendant ownership needs the private manifest; only same-root callers
        ;; may proceed to that check.
        (let* ((agent (tool-context-agent context))
               (viewer-root (if agent (task-parent-root-conversation-identifier agent)
                                (conversation-identifier (tool-context-conversation context)))))
          (unless (equal root viewer-root)
            (error 'resource-access-denied :uri uri :operation ':read))))
      (let* ((directory (merge-pathnames
                         (format nil "~A/shell-log/~A/" execution artifact)
                         (task--artifact-group-root configuration root)))
             (manifest-path (shell-log--manifest-path directory))
             (metadata
               (progn
                 (shell-log--safe-path configuration directory)
                 (if (probe-file manifest-path)
                     (shell-log--read-manifest configuration directory :measure-p nil)
                     (list :root-conversation root :execution-id execution
                           :artifact-id artifact :state ':missing)))))
        (unless (and (equal root (getf metadata :root-conversation))
                     (equal execution (getf metadata :execution-id))
                     (equal artifact (getf metadata :artifact-id))
                     (shell-log--authorized-p metadata context))
          (error 'resource-access-denied :uri uri :operation ':read))
        (unless (eq (getf metadata :state) ':missing)
          (shell-log--measure-captures configuration directory metadata))
        (make-instance 'shell-log-resource :uri uri :directory directory
                       :metadata metadata
                       :stream (if (string= stream "output") ':output ':error))))))

(defmethod resource-capabilities ((resource shell-log-resource) (context tool-context))
  "Expose only authorized bounded reads."
  (declare (ignore resource context))
  '(:read))

(-> shell-log-resource--capture (shell-log-resource) list)
(defun shell-log-resource--capture (resource)
  "Return the selected stream's portable evidence."
  (find (shell-log-resource-stream resource)
        (getf (shell-log-resource-metadata resource) :captures)
        :key (lambda (capture) (getf capture :stream))))

(-> shell-log-resource--read-bytes
    (configuration pathname &key (:offset integer) (:count integer))
    (values vector integer integer))
(defun shell-log-resource--read-bytes (configuration path &key (offset 0) (count 4096))
  "Read at most COUNT unchanged raw bytes from OFFSET without following links.
Return the octets, current file size and actual next byte position."
  (shell-log--safe-path configuration path)
  (multiple-value-bind (stream status)
      (platform-open-regular-file *platform* path :follow-links-p nil)
    (declare (ignore status))
    (unwind-protect
         (let* ((size (file-length stream))
                (start (min size offset))
                (bytes (make-array (min count (- size start))
                                   :element-type '(unsigned-byte 8))))
           (file-position stream start)
           (let ((read (read-sequence bytes stream)))
             (values (if (= read (length bytes)) bytes (subseq bytes 0 read))
                     size (+ start read))))
      (close stream))))

(-> shell-log-resource--integer
    (hash-table string &key (:default integer) (:maximum integer)) integer)
(defun shell-log-resource--integer (arguments key &key (default 0) maximum)
  "Validate a nonnegative byte window or positive result bound."
  (let ((value (gethash key arguments default)))
    (unless (and (integerp value) (<= 0 value maximum))
      (error 'tool-error :tool-name "resource.read"
             :message (format nil "~A must be an integer between 0 and ~D." key maximum)))
    value))

(-> shell-log-resource--search
    (configuration pathname &key (:query string) (:offset integer) (:maximum integer)) list)
(defun shell-log-resource--search (configuration path &key query (offset 0) (maximum 20))
  "Search raw UTF-8 query bytes within a finite scan budget, independent of lines.
The next offset resumes without skipping matches spanning the scan boundary."
  (let* ((needle (utf8-string-to-octets query))
         (length (length needle)))
    (unless (<= 1 length 256)
      (error 'tool-error :tool-name "resource.read"
             :message "Shell log search query must contain 1 to 256 UTF-8 bytes."))
    (multiple-value-bind (bytes size end)
        (shell-log-resource--read-bytes
         configuration path :offset offset
         :count (+ *shell-log-resource-scan-bytes* (1- length)))
      (let* ((start (min offset size))
             (scan-end (min (length bytes) *shell-log-resource-scan-bytes*))
             (position 0)
             (next (+ start scan-end))
             (matches nil))
        (loop for match = (search needle bytes :start2 position)
              while (and match (< match scan-end) (< (length matches) maximum))
              do (let ((excerpt-start (max 0 (- match 64)))
                       (excerpt-end (min (length bytes) (+ match 136))))
                   (push (list :byte-offset (+ start match)
                               :text (cl-exec-sandbox:decode-capture-bytes
                                      (subseq bytes excerpt-start excerpt-end))) matches)
                   (setf position (1+ match))
                   (when (= (length matches) maximum)
                     (setf next (+ start position)) (return))))
        (values (list :matches (nreverse matches) :byte-count size
                      :scanned-byte-count (- end start)
                      :next-byte-offset (and (< next size) next)))))))

(defmethod resource-tool-read
    ((resource shell-log-resource) (tool resource-read-tool)
     (context tool-context) (arguments hash-table))
  "Read one byte window or finite search page without conversation overflow."
  (declare (ignore tool))
  (dolist (key '("start-line" "line-count" "start-sequence" "record-count"))
    (when (nth-value 1 (gethash key arguments))
      (error 'tool-error :tool-name "resource.read"
             :message "Shell logs page by byte-offset and byte-count.")))
  (let* ((metadata (shell-log-resource-metadata resource))
         (capture (shell-log-resource--capture resource))
         (configuration (tool-context-configuration context))
         (offset (shell-log-resource--integer arguments "byte-offset"
                                              :default 0 :maximum most-positive-fixnum))
         (count (shell-log-resource--integer arguments "byte-count" :default 4096
                                             :maximum *shell-log-resource-maximum-bytes*))
         (maximum (shell-log-resource--integer arguments "max-results" :default 20
                                               :maximum *shell-log-resource-maximum-results*))
         (query (tool-argument arguments "query"))
         (file (getf capture :file)))
    (unless (shell-log--authorized-p metadata context)
      (error 'resource-access-denied :uri (resource-uri resource) :operation ':read))
    (when (or (member (getf metadata :state) '(:missing :pruned)) (null file))
      (return-from resource-tool-read
        (tool-success (format nil "URI: ~A~%Shell log unavailable: ~A.~%Capture status: ~A"
                              (resource-uri resource) (getf metadata :state)
                              (getf capture :status)))))
    ;; A persisted filename is data, not a native path designator.
    (unless (shell-log--capture-file-p file)
      (shell-log--fail "Invalid shell capture filename in manifest."))
    (let ((path (merge-pathnames file (shell-log-resource-directory resource))))
      (shell-log--safe-path configuration path)
      (unless (probe-file path)
        (return-from resource-tool-read
          (tool-success (format nil "URI: ~A~%Shell log unavailable: missing capture file.~%Capture status: ~A"
                                (resource-uri resource) (getf capture :status)))))
      (if query
          (progn
            (unless (and (stringp query) (plusp maximum))
              (error 'tool-error :tool-name "resource.read"
                     :message "Shell log search needs a nonempty query and positive max-results."))
            (let ((result (shell-log-resource--search configuration path :query query
                                                     :offset offset :maximum maximum)))
              (tool-success
               (with-output-to-string (output)
                 (format output "URI: ~A~%Capture status: ~A; complete: ~A~%Total bytes: ~D~%Scanned bytes: ~D~%Next byte offset: ~A~%"
                         (resource-uri resource) (getf capture :status) (getf capture :complete-p)
                         (getf result :byte-count) (getf result :scanned-byte-count)
                         (getf result :next-byte-offset))
                 (dolist (match (getf result :matches))
                   (format output "~D: ~A~%" (getf match :byte-offset) (getf match :text)))))))
          (multiple-value-bind (bytes size next)
              (shell-log-resource--read-bytes configuration path :offset offset :count count)
            (tool-success
             (format nil "URI: ~A~%Capture status: ~A; complete: ~A; truncated: ~A~%Total retained bytes: ~D; observed bytes: ~A~%Byte offset: ~D; returned bytes: ~D~%Next byte offset: ~A~%Content:~%~A"
                     (resource-uri resource) (getf capture :status) (getf capture :complete-p)
                     (getf capture :truncated-p) size
                     (if (getf capture :observed-byte-count-known-p t)
                         (getf capture :observed-byte-count 0)
                         "unknown (capture interrupted or still active)")
                     (min size offset) (length bytes) (and (< next size) next)
                     (cl-exec-sandbox:decode-capture-bytes bytes))))))))
