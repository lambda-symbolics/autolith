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
  (values
   (record-check value :properties-p t :allow-other-keys nil
                 :fields (mapcar (lambda (key)
                                   (list :indicator key :required t))
                                 keys))))

(defparameter *data-transfer-maximum-archive-bytes* (* 1024 1024 1024)
  "The largest archive read into memory by one transfer.")

(-> data-transfer--archive-atom-p (t) boolean)
(defun data-transfer--archive-atom-p (value)
  "Return true for an atom an archive may hold: NIL, T, keywords, strings, numbers or octets."
  (or (null value) (eq value t) (keywordp value) (stringp value) (numberp value)
      (typep value '(vector (unsigned-byte 8)))))

(-> data-transfer--archive-grammar () source-grammar)
(defun data-transfer--archive-grammar ()
  "Return the sexp-config grammar of exactly what DATA-TRANSFER--FORMS-BYTES prints.

The printer writes NIL and T as COMMON-LISP:NIL and COMMON-LISP:T and unmarked
single floats, and every source node needs at least one octet."
  (make-source-grammar :label                                     "The archive"
                       :maximum-depth                             128
                       :maximum-nodes                             *data-transfer-maximum-archive-bytes*
                       :allowed-atom-predicate                    #'data-transfer--archive-atom-p
                       :qualified-common-lisp-symbols-permitted-p t
                       :octet-vectors-permitted-p                 t
                       :read-default-float-format                 'single-float))

(-> data-transfer--portable-p (t) boolean)
(defun data-transfer--portable-p (value)
  "Return true when VALUE is finite archive data the archive grammar reads back."
  (handler-case
      (progn
        (validate-tree value (data-transfer--archive-grammar))
        t)
    (sexp-config-error ()
      nil)))

(-> data-transfer--read (pathname) list)
(defun data-transfer--read (pathname)
  "Read one bounded portable archive form without constructors, evaluation, or reader labels."
  (let ((form (handler-case
                  (read-source-file pathname (data-transfer--archive-grammar)
                                    :maximum-octets *data-transfer-maximum-archive-bytes*)
                (sexp-config-error (condition)
                  (data-transfer--fail pathname ':invalid
                                       (sexp-config-error-message condition))))))
    (unless (listp form)
      (data-transfer--fail pathname ':invalid "Expected one portable archive form."))
    form))

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
  (utf8-string-to-octets
   (with-output-to-string (stream)
     (let ((*print-readably* nil) (*print-escape* t) (*print-array* t)
           (*print-pretty* nil) (*print-circle* nil)
           (*print-level* nil) (*print-length* nil)
           (*package* (find-package '#:keyword)))
       (dolist (form forms)
         (write form :stream stream)
         (terpri stream))))))

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

(-> data-transfer--publish-files (list &key (:roots list)) integer)
(defun data-transfer--publish-files (writes &key roots)
  "Publish WRITES, plists of :PATH, :BYTES and :OLD, as one sexp-store transaction.

ROOTS pairs each path's namestring with the trusted root its path check uses.
Conflicts and incomplete rollbacks become typed transfer failures."
  (handler-case
      (files-publish
       (mapcar (lambda (write)
                 (list :pathname (getf write :path)
                       :octets   (coerce (getf write :bytes) '(vector (unsigned-byte 8)))
                       :expected (and (getf write :old)
                                      (coerce (getf write :old) '(vector (unsigned-byte 8))))))
               writes)
       :check (lambda (pathname)
                (let ((root (rest (assoc (namestring pathname) roots :test #'string=))))
                  (when root
                    (data-transfer--check-path root pathname)))))
    (publication-rollback-failed (condition)
      (error 'data-transfer-rollback-error
             :pathname nil
             :reason ':rollback
             :failures (publication-rollback-failed-failures condition)
             :message (format nil "Import failed and some restorations failed. Retained recovery copies: ~S"
                              (publication-rollback-failed-failures condition))))
    (publication-conflict (condition)
      (data-transfer--fail (store-error-pathname condition) ':conflict
                           "The destination changed during the transfer."))))

(-> data-transfer--write-export (pathname list) pathname)
(defun data-transfer--write-export (pathname archive)
  "Atomically publish a new private ARCHIVE at PATHNAME."
  (handler-case
      (snapshot-write-octets
       pathname
       (data-transfer--forms-bytes (list archive))
       :require-absent t)
    (publication-conflict (condition)
      (data-transfer--fail (store-error-pathname condition) ':conflict
                           "The export destination already exists.")))
  pathname)

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
  (workspace-name-identifier directory))

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
        :facts (count :claim-asserted (getf archive :knowledge) :key #'first)
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
