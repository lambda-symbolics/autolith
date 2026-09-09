(in-package #:autolith)

;;;; -- Portable User Data --

(defparameter *data-transfer-version* 1
  "The portable user-data archive version.")

(defvar *data-transfer-lock* (make-recursive-lock "Autolith data transfer")
  "Serialize exports and imports in this process.")

(define-condition data-transfer-error (autolith-error)
  ((pathname
    :initarg :pathname
    :reader data-transfer-error-pathname
    :documentation "The archive or durable pathname involved in the failure.")
   (reason
    :initarg :reason
    :reader data-transfer-error-reason
    :documentation "A machine-readable failure category."))
  (:documentation "Portable data failed validation, collection, or publication.")
  (:report (lambda (condition stream)
             (format stream "Data transfer ~A at ~A: ~A"
                     (data-transfer-error-reason condition)
                     (data-transfer-error-pathname condition)
                     (autolith-error-message condition)))))

(define-condition data-transfer-conflict (data-transfer-error) ()
  (:documentation "An imported identity conflicts with existing or live data."))

(define-condition data-transfer-rollback-error (data-transfer-error)
  ((failures :initarg :failures :reader data-transfer-rollback-error-failures
             :documentation "Failed restorations and retained private backup paths."))
  (:documentation "Import rollback could not restore every destination; recovery copies are retained."))

(-> data-transfer--fail (t keyword string) nil)
(defun data-transfer--fail (pathname reason message)
  "Signal a typed transfer failure with PATHNAME, REASON, and MESSAGE."
  (error (if (eq reason ':conflict)
             'data-transfer-conflict
             'data-transfer-error)
         :pathname pathname :reason reason :message message))

(-> data-transfer--configuration () configuration)
(defun data-transfer--configuration ()
  "Return the active session configuration, or a deferred CLI configuration."
  (let ((symbol (find-symbol "*ACTIVE-APPLICATION*" '#:autolith)))
    (if (and symbol (boundp symbol) (symbol-value symbol))
        (application-configuration (symbol-value symbol))
        (configuration-create :defer-provider-validation-p t))))

(-> data-transfer--properties-p (t list) boolean)
(defun data-transfer--properties-p (value keys)
  "Return true for a proper plist containing each of KEYS exactly once."
  (handler-case
      (and (listp value)
           (eql (list-length value) (* 2 (length keys)))
           (let ((actual (loop for tail on value by #'cddr collect (first tail))))
             (and (every (lambda (key) (member key keys)) actual)
                  (= (length actual) (length (remove-duplicates actual))))))
    (error () nil)))

(-> data-transfer--portable-p (t) boolean)
(defun data-transfer--portable-p (value)
  "Accept finite portable data, rejecting reader-created executable objects."
  (let ((pending (list value))
        (seen (make-hash-table :test #'eq)))
    (loop while pending
          for object = (pop pending)
          do (cond
               ((or (null object) (eq object t) (keywordp object)
                    (stringp object) (numberp object) (characterp object)))
               ((gethash object seen)
                (return-from data-transfer--portable-p nil))
               ((consp object)
                (setf (gethash object seen) t)
                (push (first object) pending)
                (push (rest object) pending))
               ((and (vectorp object)
                     (every (lambda (byte) (typep byte '(unsigned-byte 8))) object)))
               (t
                (return-from data-transfer--portable-p nil))))
    t))

(-> data-transfer--reject-reader (stream character t) nil)
(defun data-transfer--reject-reader (stream character argument)
  "Reject dispatch syntax that could construct or evaluate nonportable objects."
  (declare (ignore stream argument))
  (data-transfer--fail nil ':invalid
                       (format nil "Reader dispatch #~A is not allowed." character)))

(defparameter *data-transfer-maximum-archive-bytes* (* 1024 1024 1024)
  "The largest archive read into memory by one transfer.")

(-> data-transfer--read (pathname) list)
(defun data-transfer--read (pathname)
  "Read bounded portable syntax without constructors, evaluation, or reader labels."
  (with-open-file (stream pathname :element-type '(unsigned-byte 8))
    (when (> (file-length stream) *data-transfer-maximum-archive-bytes*)
      (data-transfer--fail pathname ':invalid "The archive exceeds the transfer size limit.")))
  (let* ((*read-eval* nil) (*readtable* (copy-readtable nil))
         (*package* (find-package '#:keyword))
         (depth 0) (list-reader (get-macro-character #\( *readtable*)))
    (flet ((enter ()
             (when (> (incf depth) 128)
               (data-transfer--fail pathname ':invalid "Archive nesting exceeds 128 levels."))))
      (loop for code from 33 below 127
            for character = (code-char code)
            unless (or (digit-char-p character) (find character "(\\")) do
              (set-dispatch-macro-character #\# character #'data-transfer--reject-reader))
      (set-macro-character
       #\( (lambda (stream character)
              (enter)
              (unwind-protect (funcall list-reader stream character) (decf depth))))
      (set-dispatch-macro-character
       #\# #\( (lambda (stream character length)
                  (declare (ignore character))
                  (when length
                    (data-transfer--fail pathname ':invalid "Length-prefixed vectors are not allowed."))
                  (enter)
                  (unwind-protect
                       (let ((elements (read-delimited-list #\) stream t)))
                         (unless (every (lambda (byte) (typep byte '(unsigned-byte 8))) elements)
                           (data-transfer--fail pathname ':invalid "Archive vectors contain only octets."))
                         (coerce elements '(vector (unsigned-byte 8))))
                    (decf depth))))
      (handler-case
          (with-open-file (stream pathname :external-format ':utf-8)
            (let* ((end (gensym "END")) (form (read stream nil end)))
              (unless (and (not (eq form end)) (eq (read stream nil end) end)
                           (data-transfer--portable-p form))
                (data-transfer--fail pathname ':invalid "Expected one portable archive form."))
              form))
        (data-transfer-error (condition)
          (error condition))
        (error (condition)
          (data-transfer--fail pathname ':invalid (princ-to-string condition)))))))

(-> data-transfer--bytes (pathname) vector)
(defun data-transfer--bytes (pathname)
  "Read the complete binary contents of a regular private file."
  (with-open-file (stream pathname :element-type '(unsigned-byte 8))
    (let ((bytes (make-array (file-length stream) :element-type '(unsigned-byte 8))))
      (unless (= (read-sequence bytes stream) (length bytes))
        (data-transfer--fail pathname ':changed "File changed while reading."))
      bytes)))

(-> data-transfer--forms-bytes (list) vector)
(defun data-transfer--forms-bytes (forms)
  "Serialize complete FORMS with portable readable syntax and UTF-8."
  (sb-ext:string-to-octets
   (with-output-to-string (stream)
     (let ((*print-readably* nil) (*print-escape* t) (*print-array* t)
           (*print-pretty* nil) (*print-circle* nil)
           (*print-level* nil) (*print-length* nil)
           (*package* (find-package '#:keyword)))
       (dolist (form forms)
         (write form :stream stream)
         (terpri stream))))
   :external-format ':utf-8))

(-> data-transfer--safe-component-p (t) boolean)
(defun data-transfer--safe-component-p (value)
  "Return true for one literal portable path component."
  (and (conversation-identifier-path-component-p value)
       (not (find #\: value))
       (notany (lambda (character) (< (char-code character) 32)) value)
       t))

(-> data-transfer--native-component-p (t) boolean)
(defun data-transfer--native-component-p (value)
  "Accept one literal native filename, including backslashes and wildcard characters."
  (and (non-empty-string-p value)
       (not (member value '("." "..") :test #'equal))
       (not (find #\/ value)) (not (find #\Null value)) t))

(-> data-transfer--directory-pathname (pathname) pathname)
(defun data-transfer--directory-pathname (pathname)
  "Convert a literal pathname to directory form without reinterpreting filename escapes."
  (if (uiop:directory-pathname-p pathname)
      pathname
      (uiop:parse-native-namestring
       (concatenate 'string (uiop:native-namestring pathname) "/"))))

(-> data-transfer--relative-components (pathname pathname) list)
(defun data-transfer--relative-components (pathname root)
  "Return PATHNAME's literal components below directory ROOT, rejecting escapes.

The components come from the parsed pathname rather than from a native
namestring, so no host directory separator ever has to be split out of a name."
  (let* ((directory (data-transfer--directory-pathname root))
         (relative (uiop:enough-pathname pathname directory))
         (file (uiop:native-namestring
                (make-pathname :host nil :device nil :directory nil
                               :defaults relative))))
    (when (uiop:absolute-pathname-p relative)
      (data-transfer--fail pathname ':invalid "Path escapes the data root."))
    (append (rest (pathname-directory relative))
            (if (string= file "")
                nil
                (list file)))))

(-> data-transfer--safe-path-p (t) boolean)
(defun data-transfer--safe-path-p (path)
  "Return true for a nonempty proper relative component list."
  (and (listp path) (integerp (list-length path)) path
       (every #'data-transfer--native-component-p path) t))

(-> data-transfer--path (pathname list) pathname)
(defun data-transfer--path (root components)
  "Construct a confined pathname from validated literal COMPONENTS."
  (unless (data-transfer--safe-path-p components)
    (data-transfer--fail root ':invalid "Invalid relative archive path."))
  (merge-pathnames
   (uiop:parse-native-namestring (format nil "~{~A~^/~}" components)) root))

(-> data-transfer--check-path (pathname pathname) null)
(defun data-transfer--check-path (root pathname)
  "Reject symbolic links and nonregular files below the trusted ROOT."
  (let ((parts (data-transfer--relative-components pathname root))
        (current root))
    (unless (every #'data-transfer--native-component-p parts)
      (data-transfer--fail pathname ':invalid "Path escapes the data root."))
    (dolist (part parts)
      (setf current (merge-pathnames (uiop:parse-native-namestring part) current))
      (let ((status (platform-path-status *platform* current)))
        (when (and status
                   (not (member (platform-file-status-kind status)
                                '(:directory :file))))
          (data-transfer--fail current ':invalid "Symbolic links and special files are not transferable.")))
      (setf current (data-transfer--directory-pathname current))))
  nil)

(-> data-transfer--private-write (pathname vector) pathname)
(defun data-transfer--private-write (pathname bytes)
  "Write BYTES to a new private file, never following an existing path."
  (let ((stream (platform-create-private-file *platform* pathname)))
    (unwind-protect
         (progn
           (write-sequence bytes stream)
           (finish-output stream))
      (close stream)))
  pathname)

(-> data-transfer--publish (pathname pathname boolean) pathname)
(defun data-transfer--publish (temporary target replace-p)
  "Publish TEMPORARY atomically, refusing an occupied new TARGET."
  (if replace-p
      (uiop:rename-file-overwriting-target temporary target)
      (platform-publish-new-file *platform* temporary target))
  target)

(-> data-transfer--temporary (pathname) pathname)
(defun data-transfer--temporary (target)
  "Return an unpredictable private staging pathname beside TARGET."
  (merge-pathnames (format nil ".data-transfer-~A" (make-identifier))
                   (uiop:pathname-directory-pathname target)))

(-> data-transfer--write-export (pathname list) pathname)
(defun data-transfer--write-export (pathname archive)
  "Atomically publish a new private ARCHIVE at PATHNAME."
  (let ((temporary (data-transfer--temporary pathname)))
    (unwind-protect
         (progn
           (data-transfer--private-write temporary
                                         (data-transfer--forms-bytes (list archive)))
           (data-transfer--publish temporary pathname nil))
      (when (probe-file temporary) (delete-file temporary)))))

(-> data-transfer--workspace-name (t) (option string))
(defun data-transfer--workspace-name (directory)
  "Canonicalize an external workspace argument, allowing absent directories."
  (when directory
    (let ((path (data-transfer--directory-pathname (pathname directory))))
      (unless (uiop:absolute-pathname-p path)
        (setf path (merge-pathnames path *default-pathname-defaults*)))
      (namestring (or (ignore-errors (platform-truename *platform* path)) path)))))

(-> data-transfer--workspace-identifier (string) string)
(defun data-transfer--workspace-identifier (directory)
  "Hash a canonical stored workspace key even when its directory is absent."
  (let ((mac (make-mac ':siphash *workspace-directory-identifier-key*
                       :digest-length 16)))
    (update-mac mac (sb-ext:string-to-octets directory :external-format ':utf-8))
    (with-output-to-string (stream)
      (loop for byte across (produce-mac mac) do (format stream "~2,'0x" byte)))))

(-> data-transfer--report (pathname list) list)
(defun data-transfer--report (pathname archive)
  "Return portable archive scope and entity counts."
  (list :pathname (namestring pathname)
        :workspace (getf archive :workspace)
        :workspaces (getf archive :workspaces)
        :conversations (count ':conversation (getf archive :sessions)
                              :key (lambda (entry) (getf entry :kind)))
        :inferences (count ':inference (getf archive :sessions)
                           :key (lambda (entry) (getf entry :kind)))
        :memories (length (data-transfer--histories (getf archive :memories)))
        :papercuts (length (data-transfer--histories (getf archive :papercuts)))
        :agendas (length (getf archive :agendas))
        :plans (length (getf archive :plans))
        :files (length (getf archive :files))))

(-> data-transfer--native-pathname ((or pathname string)) pathname)
(defun data-transfer--native-pathname (value)
  "Interpret string arguments as native names and preserve pathname arguments."
  (etypecase value
    (pathname value)
    (string (uiop:parse-native-namestring value))))

(-> data-export ((or pathname string) &key (:workspace t) (:configuration configuration)) list)
(defun data-export (pathname &key workspace (configuration (data-transfer--configuration)))
  "Export all portable user data, or one WORKSPACE, to a new private archive.

Returns a portable plist with the archive path, workspace keys, and counts.
Credentials, configuration, executable image state, and caches are excluded."
  (let* ((pathname (uiop:ensure-absolute-pathname (data-transfer--native-pathname pathname)))
         (workspace (and workspace (data-transfer--workspace-name
                                    (data-transfer--native-pathname workspace)))))
    (data-transfer--call-with-locks
     configuration
     (lambda ()
       (let ((archive (data-transfer--collect configuration workspace)))
         (data-transfer--validate archive pathname)
         (data-transfer--write-export pathname archive)
         (data-transfer--report pathname archive))))))

(-> data-import ((or pathname string) &key (:workspace t) (:configuration configuration)) list)
(defun data-import (pathname &key workspace (configuration (data-transfer--configuration)))
  "Merge a portable archive, optionally relocating its single WORKSPACE.

Identical identities are skipped; conflicts fail before publication. Ordinary
publication failures roll back all files written by this invocation. Returns
archive counts and the number of files installed. Imported code is never run."
  (let* ((pathname (uiop:ensure-absolute-pathname (data-transfer--native-pathname pathname)))
         (archive (data-transfer--read pathname)))
    (data-transfer--validate archive pathname)
    (when workspace
      (unless (getf archive :workspace)
        (data-transfer--fail pathname ':invalid
                             "Workspace remapping requires a workspace archive."))
      (setf archive (data-transfer--remap
                     archive (data-transfer--workspace-name (data-transfer--native-pathname workspace))))
      (data-transfer--validate archive pathname))
    (data-transfer--call-with-locks
     configuration
     (lambda ()
       (let ((installed (data-transfer--install configuration archive)))
         (append (data-transfer--report pathname archive)
                 (list :installed-files installed)))))))
