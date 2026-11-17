(in-package #:autolith)

;;;; -- Structured Evidence Dependencies on Managed Captures --

(defvar *shell-log-evidence-pins* nil
  "Containing artifact directories protected by the locked retention snapshot.")

(defvar *shell-log-legacy-evidence-references* nil
  "Exact old-format evidence references prepared outside retention locks.")

(-> shell-log--reference-parts (t) (option list))
(defun shell-log--reference-parts (reference)
  "Parse only the canonical opaque stream identity, without accessing storage."
  (when (and (stringp reference) (uiop:string-prefix-p "shell-log:" reference))
    (let ((parts (uiop:split-string (subseq reference 10) :separator "/")))
      (when (and (= (length parts) 4)
                 (every #'shell-log--component-p parts)
                 (member (fourth parts) '("output" "error") :test #'string=))
        parts))))

(-> shell-log-evidence-references-p (t) boolean)
(defun shell-log-evidence-references-p (references)
  "Recognize a bounded set of canonical stream identities in a tool source."
  (and (proper-list-p references)
       (<= (length references) 32)
       (every #'shell-log--reference-parts references)
       t))

(-> shell-log--validated-reference
    (configuration string &key (:require-present-p boolean))
    (values pathname list))
(defun shell-log--validated-reference (configuration reference &key (require-present-p t))
  "Return REFERENCE's exact containing directory and validated manifest.

Existing pins may name pruned or missing storage. New evidence must name an
existing regular capture in the exact manifest, never another artifact's URI."
  (let ((parts (shell-log--reference-parts reference)))
    (unless parts
      (knowledge--fail ':invalid-shell-reference reference "Malformed managed stream reference."))
    (destructuring-bind (root execution artifact stream-name) parts
      (let* ((directory (merge-pathnames
                         (format nil "~A/shell-log/~A/" execution artifact)
                         (task--artifact-group-root configuration root)))
             (manifest  (shell-log--manifest-path directory))
             (stream    (if (string= stream-name "output") ':output ':error)))
        (shell-log--safe-path configuration directory)
        (unless (probe-file manifest)
          (if require-present-p
              (knowledge--fail ':unavailable-shell-reference reference
                               "The managed stream has no retained manifest.")
              (return-from shell-log--validated-reference (values directory nil))))
        (let* ((metadata (shell-log--read-manifest configuration directory :measure-p nil))
               (capture  (find stream (getf metadata :captures)
                               :key (lambda (capture) (getf capture :stream)))))
          (unless (and (equal root (getf metadata :root-conversation))
                       (equal execution (getf metadata :execution-id))
                       (equal artifact (getf metadata :artifact-id)))
            (knowledge--fail ':invalid-shell-reference reference
                             "Managed stream and manifest identities disagree."))
          (when require-present-p
            (unless (and capture (not (eq (getf metadata :state) ':pruned))
                         (shell-log--capture-file-p (getf capture :file)))
              (knowledge--fail ':unavailable-shell-reference reference
                               "The managed stream is not retained."))
            (let* ((path   (shell-log--safe-path
                            configuration (merge-pathnames (getf capture :file) directory)))
                   (status (platform-path-status *platform* path :follow-links-p nil)))
              (unless (and status (eq (platform-file-status-kind status) ':file))
                (knowledge--fail ':unavailable-shell-reference reference
                                 "The managed capture is not a regular retained file."))))
          (values directory metadata))))))

(-> shell-log-result-evidence-references
    (configuration t &key (:require-present-p boolean)) list)
(defun shell-log-result-evidence-references (configuration result &key (require-present-p t))
  "Extract exact identities from actual RESULT details, validating their manifests."
  (let* ((details    (tool-result-details result))
         (logs       (and (proper-list-p details) (evenp (length details))
                          (getf details :shell-logs)))
         (references nil))
    (unless (and (proper-list-p logs) (<= (length logs) 16))
      (knowledge--fail ':invalid-shell-reference logs "Tool shell metadata is not bounded."))
    (dolist (log logs)
      (unless (and (proper-list-p log) (evenp (length log))
                   (proper-list-p (getf log :captures))
                   (<= (length (getf log :captures)) 2))
        (knowledge--fail ':invalid-shell-reference log "Malformed tool capture metadata."))
      (dolist (capture (getf log :captures))
        (unless (and (proper-list-p capture) (evenp (length capture)))
          (knowledge--fail ':invalid-shell-reference capture "Malformed tool stream metadata."))
        (let* ((reference (getf capture :reference))
               (parts     (shell-log--reference-parts reference)))
          (unless (and parts
                       (equal (second parts) (getf log :execution-id))
                       (equal (third parts) (getf log :artifact-id))
                       (eq (getf capture :stream)
                           (if (string= (fourth parts) "output") ':output ':error)))
            (knowledge--fail ':invalid-shell-reference reference
                             "Tool stream metadata and its reference disagree."))
          (shell-log--validated-reference configuration reference :require-present-p require-present-p)
          (pushnew reference references :test #'equal))))
    (nreverse references)))

(-> shell-log--file-evidence-reference (configuration list &key (:require-present-p boolean))
    (option string))
(defun shell-log--file-evidence-reference (configuration evidence &key (require-present-p nil))
  "Return an exact managed capture identity for FILE evidence, or NIL for other files."
  (let* ((source     (getf evidence :source))
         (properties (rest source))
         (path       (and (eq (first source) :file)
                          (if (getf properties :relative)
                              (and (getf evidence :workspace)
                                   (merge-pathnames (getf properties :path)
                                                    (uiop:ensure-directory-pathname
                                                     (getf evidence :workspace))))
                              (pathname (getf properties :path)))))
         (root       (merge-pathnames "tasks/" (config :data-root configuration)))
         (relative   (and path (uiop:subpathp path root)))
         (components (and relative (rest (pathname-directory relative)))))
    (when (and (= (length components) 4)
               (equal (third components) "shell-log")
               (every #'shell-log--component-p
                      (list (first components) (second components) (fourth components))))
      (let* ((directory (uiop:pathname-directory-pathname path))
             (manifest  (shell-log--manifest-path directory)))
        (shell-log--safe-path configuration path)
        (when (and require-present-p (not (probe-file manifest)))
          (knowledge--fail ':unavailable-shell-reference (namestring path)
                           "Managed FILE evidence has no retained capture manifest."))
        (when (probe-file manifest)
          (let* ((metadata (shell-log--read-manifest configuration directory :measure-p nil))
                 (capture  (find (file-namestring path) (getf metadata :captures)
                                 :key (lambda (capture) (getf capture :file)) :test #'equal)))
            (when (and require-present-p (not capture))
              (knowledge--fail ':invalid-shell-reference (namestring path)
                               "Managed FILE evidence does not name a manifest capture."))
            (when capture
              (let* ((artifact  (make-instance 'shell-log-artifact :configuration configuration
                                              :capture-directory directory :metadata metadata))
                     (reference (shell-log-reference artifact (getf capture :stream))))
                (unless (equal directory
                               (shell-log--validated-reference
                                configuration reference :require-present-p require-present-p))
                  (knowledge--fail ':invalid-shell-reference path
                                   "File evidence is outside the named managed artifact."))
                reference))))))))

(-> shell-log--evidence-references (configuration list &key (:require-present-p boolean)) list)
(defun shell-log--evidence-references (configuration evidence &key require-present-p)
  "Project declared tool references or an exact managed FILE dependency."
  (let ((source (getf evidence :source)))
    (case (first source)
      (:tool
       (let ((references (getf (rest source) :shell-logs)))
         (unless (shell-log-evidence-references-p references)
           (knowledge--fail ':invalid-shell-reference source "Malformed stored stream dependencies."))
         (dolist (reference references)
           (shell-log--validated-reference configuration reference :require-present-p require-present-p))
         references))
      (:file
       (let ((reference (shell-log--file-evidence-reference
                         configuration evidence :require-present-p require-present-p)))
         (when reference (list reference)))))))

(-> shell-log-validate-evidence-records (configuration knowledge-base list) null)
(defun shell-log-validate-evidence-records (configuration base records)
  "Validate planned raw dependencies while knowledge locks exclude pruning.
Request old evidence attribution outside these locks before retrying a cold write."
  (let ((evidence (make-hash-table :test #'equal))
        (items    nil)
        (legacy   nil))
    (dolist (record records)
      (when (eq (first record) ':evidence)
        (setf (gethash (getf (rest record) :id) evidence) (rest record))))
    (dolist (record records)
      (let* ((properties (rest record))
             (identifier (case (first record)
                           (:evidence
                            (getf properties :id))
                           (:evidence-renewed
                            (getf properties :id))
                           (:claim-asserted
                            (getf properties :evidence))))
             (item       (and identifier (or (gethash identifier evidence)
                                             (knowledge-base-evidence-record base identifier)))))
        (when item (pushnew item items :key (lambda (item) (getf item :id)) :test #'equal))))
    (dolist (item items)
      (shell-log--evidence-references configuration item :require-present-p t)
      (let ((source (getf item :source)))
        (when (and (eq (first source) :tool)
                   (not (get-properties (rest source) '(:shell-logs))))
          (unless (knowledge-base-evidence-record base (getf item :id))
            (knowledge--fail ':unresolved-legacy-shell-evidence (getf item :id)
                             "A new tool observation must declare its raw dependencies."))
          (push item legacy))))
    (dolist (item legacy)
      (multiple-value-bind (references warmed-map)
          (handler-case (shell-log-legacy-evidence-cached-references configuration base item)
            (knowledge-error ()
              (error 'knowledge-evidence-warm-required :base base :evidence legacy)))
        (unless (shell-log-legacy-evidence-cache-valid-p configuration base warmed-map)
          (error 'knowledge-evidence-warm-required :base base :evidence legacy))
        (dolist (reference references)
          (shell-log--validated-reference configuration reference)))))
  nil)

(-> shell-log-evidence-pins (configuration knowledge-base hash-table) hash-table)
(defun shell-log-evidence-pins (configuration base legacy-references)
  "Fold exact containing-artifact pins from every nonexpired, unretracted support.

All workspaces and future-valid supports participate. A copied excerpt or an
uncited resident evidence record does not establish a raw dependency."
  (let ((pins (make-hash-table :test #'equal))
        (seen (make-hash-table :test #'equal))
        (now  (knowledge-now)))
    (maphash
     (lambda (identifier support)
       (declare (ignore identifier))
       (let* ((evidence-id (getf support :evidence))
              (evidence    (and evidence-id
                                (knowledge-base-evidence-record base evidence-id))))
         (when (and evidence (not (gethash evidence-id seen))
                    (null (getf support :retracted-at))
                    (or (null (getf support :valid-until))
                        (<= now (getf support :valid-until)))
                    (or (null (getf evidence :valid-until))
                        (<= now (getf evidence :valid-until))))
           (setf (gethash evidence-id seen) t)
           (dolist (reference (append (shell-log--evidence-references configuration evidence)
                                     (gethash evidence-id legacy-references)))
             (setf (gethash (namestring (shell-log--validated-reference
                                         configuration reference :require-present-p nil)) pins)
                   t)))))
     (knowledge-base-supports base))
    pins))

(-> shell-log-call-with-retention-lock (configuration function) t)
(defun shell-log-call-with-retention-lock (configuration function)
  "Call FUNCTION under knowledge-before-shell locks with authoritative evidence pins.

Legacy transcript reads and conversation source locks occur only in the warm
phase. A changed base or source stamp retries outside both retention locks."
  (loop
    (let* ((snapshot (knowledge-base-read configuration))
           (legacy   (shell-log-legacy-evidence-references configuration snapshot))
           (retry-p  nil)
           (result
             (knowledge-call-with-locked-base
              configuration
              (lambda (base)
                (if (and (or (eq base snapshot)
                             ;; A missing knowledge log yields a fresh empty base on each read.
                             (and (zerop (knowledge-base-revision base))
                                  (zerop (knowledge-base-revision snapshot))
                                  (equal (knowledge-base-pathname base)
                                         (knowledge-base-pathname snapshot))))
                         (shell-log-legacy-evidence-cache-valid-p configuration snapshot legacy))
                    (with-shell-log-lock (configuration)
                      (let ((*shell-log-evidence-pins*
                              (shell-log-evidence-pins configuration base legacy))
                            (*shell-log-legacy-evidence-references* legacy))
                        (funcall function)))
                    (setf retry-p t))))))
      (unless retry-p (return result)))))
