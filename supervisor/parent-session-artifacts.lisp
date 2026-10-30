;;;; -- Approved Legacy Artifact Projection --

(in-package #:autolith-supervisor)

(serapeum/types:-> parent-session-artifact-root (parent-configuration string) pathname)
(defun parent-session-artifact-root (configuration identifier)
  "Return a session-specific public projection outside the private session tree."
  (merge-pathnames
   (format nil "session-artifacts/~A/" (parent-session--component identifier))
   (uiop:pathname-parent-directory-pathname
    (uiop:ensure-directory-pathname (parent-configuration-get configuration ':private-root)))))

(serapeum/types:-> parent-session--literal-relative (string) pathname)
(defun parent-session--literal-relative (text)
  "Accept literal relative artifact components, without wildcard or parent traversal."
  (let ((parts (uiop:split-string text :separator '(#\/))))
    (unless (and parts (every (lambda (part)
                               (and (plusp (length part))
                                    (not (member part '("." "..") :test #'equal))
                                    (every (lambda (character)
                                             (or (alphanumericp character) (find character "-_."))) part))) parts))
      (parent-session--reject ':invalid-artifact-path))
    (pathname text)))

(serapeum/types:-> parent-session--artifact-source (pathname pathname) pathname)
(defun parent-session--artifact-source (root relative)
  "Reject links in every source component before opening the regular artifact."
  (let ((current (uiop:ensure-directory-pathname root)))
    (dolist (part (remove "" (uiop:split-string (namestring relative) :separator '(#\/)) :test #'equal))
      (setf current (merge-pathnames part current))
      (let* ((information (ls-compat.posix:file-information current :follow-links-p nil))
             (kind        (ls-compat.posix:file-information-kind information)))
        (unless (member kind '(:directory :file))
          (parent-session--reject ':invalid-artifact-file))
        (when (eq kind ':directory) (setf current (uiop:ensure-directory-pathname current)))))
    current))

(serapeum/types:-> parent-session--artifact-destination (pathname pathname) pathname)
(defun parent-session--artifact-destination (root relative)
  "Reject existing links in the projection directory and every destination component."
  (let* ((base    (uiop:pathname-parent-directory-pathname
                   (uiop:pathname-parent-directory-pathname root)))
         (path    (merge-pathnames relative root))
         (parts   (remove "" (uiop:split-string (enough-namestring path base) :separator '(#\/)) :test #'equal))
         (current base))
    (loop for part in parts for final-p = (eq part (first (last parts))) do
      (setf current (merge-pathnames part current))
      (let ((information
              (handler-case (ls-compat.posix:file-information current :follow-links-p nil)
                (ls-compat.posix:file-operation-failed () nil))))
        (when (and information
                   (not (eq (ls-compat.posix:file-information-kind information)
                            (if final-p ':file ':directory))))
          (parent-session--reject ':invalid-artifact-destination)))
      (unless final-p (setf current (uiop:ensure-directory-pathname current))))
    path))

(serapeum/types:-> parent-session-project-artifacts
                   (parent-configuration string &key (:header list) (:records list)
                    (:source-root (or null pathname))) (values list list))
(defun parent-session-project-artifacts (configuration identifier &key header records source-root)
  "Copy only referenced regular artifacts and rebase durable descriptors to their public projection.
Keep SHA256 digests in the private manifest. Never expose the original data root."
  (let* ((legacy    (pathname (parent-configuration-get configuration ':legacy-conversations-root)))
         (data-root (uiop:pathname-parent-directory-pathname (uiop:ensure-directory-pathname legacy)))
         (target    (parent-session-artifact-root configuration identifier))
         (manifest nil) (total 0) (count 0))
    (dolist (key '(:private-root :workspace :temporary-root))
      (let ((scope (parent-configuration-get configuration key)))
        (when (and scope (access-policy--overlap-p (namestring target) (namestring (pathname scope))))
          (parent-session--reject ':artifact-projection-overlap))))
    (labels ((copy-artifact (root relative destination)
               (let* ((safe        (parent-session--literal-relative relative))
                      (source      (parent-session--artifact-source root safe))
                      (destination (parent-session--artifact-destination
                                    target (parent-session--literal-relative destination))))
                 (when (> (incf count) 4096) (parent-session--reject ':artifact-limit))
                 (with-open-stream (input (ls-compat.posix:open-regular-file source))
                   (let ((size (file-length input)))
                     (when (or (> size (* 64 1024 1024)) (> (incf total size) (* 256 1024 1024)))
                       (parent-session--reject ':artifact-limit))
                     (let ((bytes (make-array size :element-type '(unsigned-byte 8))))
                       (unless (= (read-sequence bytes input) size)
                         (parent-session--reject ':artifact-changed))
                       (ensure-directories-exist destination)
                      (uiop:with-temporary-file
                          (:pathname temporary :stream output :type "bin"
                           :directory    (uiop:pathname-directory-pathname destination)
                           :element-type '(unsigned-byte 8))
                        (write-sequence bytes output)
                        (finish-output output)
                        (close output)
                        (uiop:rename-file-overwriting-target temporary destination))
                      (push (list ':artifact :path (enough-namestring destination target)
                                  :size size :sha256 (ironclad:byte-array-to-hex-string
                                                      (ironclad:digest-sequence ':sha256 bytes))) manifest))))
                 (namestring destination)))

             (visit (value)
               (cond
                 ((and (listp value) (keywordp (first value))
                       (evenp (length value)))
                  (loop for (key item) on value by (lambda (tail) (rest (rest tail))) append
                    (list key
                          (cond
                            ((and (eq key ':artifact) (stringp item) (getf value ':mime))
                             (let ((image-root (or source-root
                                                   (merge-pathnames
                                                    (format nil "conversation-images/~A/" identifier) data-root))))
                               (unless (and (member (getf value ':mime)
                                                    '("image/png" "image/jpeg" "image/webp") :test #'equal)
                                            (equal (pathname-name (parent-session--literal-relative item))
                                                   (getf value ':id)))
                                 (parent-session--reject ':invalid-image-descriptor))
                               (copy-artifact image-root item item)
                               item))
                            ((and (eq key ':source) (stringp item) (getf value ':mime))
                             (file-namestring item))
                            ((and (member key '(:artifact-path :continuity-path)) (stringp item))
                             (let* ((root     (or source-root data-root))
                                    (relative (enough-namestring (pathname item) root)))
                               (unless (or source-root (uiop:string-prefix-p "tasks/" relative))
                                 (parent-session--reject ':invalid-artifact-path))
                               (copy-artifact root relative (concatenate 'string "tools/" relative))))
                            (t
                             (visit item))))))
                 ((consp value)
                  (mapcar #'visit value))
                ((and (vectorp value) (not (stringp value)))
                 (map 'vector #'visit value))
                ((stringp value)
                 (copy-seq value))
                (t
                 value))))
      (let* ((projected-header  (visit header))
             (projected-records (mapcar (lambda (record) (cons (first record) (visit (rest record)))) records)))
        (remf (rest projected-header) ':artifact-root)
        (when manifest
          (setf (getf (rest projected-header) ':artifact-root) (namestring target))
          (sexp-store:log-write (merge-pathnames "manifest.sexp" target) (nreverse manifest)))
        (values projected-header projected-records)))))

(serapeum/types:-> parent-session-configuration (parent-configuration session-store) parent-configuration)
(defun parent-session-configuration (configuration store)
  "Add the approved projection as a read-only runtime scope before native startup."
  (let ((root (getf (rest (parent-session-legacy-header store)) ':artifact-root)))
    (if root
        (let* ((values (copy-list (parent-configuration-values configuration)))
               (copy   (make-instance 'parent-configuration :values values)))
          (unless (equal (pathname root) (parent-session-artifact-root configuration (session-store-id store)))
            (parent-session--reject ':invalid-artifact-root))
          (pushnew root (getf values ':runtime-roots) :test #'equal)
          (setf (slot-value copy 'values) values)
          (setf (parent-configuration-source-path copy) (parent-configuration-source-path configuration)
                (parent-configuration-provider-registry copy) (parent-configuration-provider-registry configuration)
                (parent-configuration-provider-paths copy) (parent-configuration-provider-paths configuration))
          copy)
        configuration)))

(serapeum/types:-> parent-session-project-fork
                   (parent-configuration string &key (:header list) (:records list)) (values list list))
(defun parent-session-project-fork (configuration identifier &key header records)
  "Copy only artifacts referenced by the selected fork prefix and rebase its tagged records."
  (let ((source-root (getf (rest header) ':artifact-root)))
    (unless source-root (return-from parent-session-project-fork (values header records)))
    (let ((locals (loop for record in records
                        when (and (eq (sixth record) ':agent) (eq (fourth record) ':note)
                                  (parent-session-record->local record))
                          collect (parent-session-record->local record))))
      (multiple-value-bind (new-header new-locals)
          (parent-session-project-artifacts configuration identifier :header header :records locals
                                            :source-root (pathname source-root))
        (values
         new-header
         (loop for record in records collect
          (if (and (eq (sixth record) ':agent) (eq (fourth record) ':note)
                   (parent-session-record->local record))
              (let* ((payload (protocol-value->data (session-store--data-value (fifth record))))
                     (local   (pop new-locals))
                     (copy    (copy-list record)))
                (setf (gethash "agent_record" payload)
                      (parent-session--tagged (parent-session--legacy-record local))
                      (fifth copy) (session-store--value-data (protocol-data->value payload)))
                copy)
              record)))))))
