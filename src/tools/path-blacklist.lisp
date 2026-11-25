(in-package #:autolith)

;;;; -- Mandatory Tool Path Policy --

(defparameter *tool-path-blacklist* nil
  "Absolute directory pathnames or strings denied to native path-taking tools.

Configure this process-wide policy in trusted init.lisp. A leading ~/ expands
against the user's home directory. Full-access approval does not override it.
This is a tool argument policy, not confinement of arbitrary Lisp or commands.")

(-> tool-path-blacklist--roots () list)
(defun tool-path-blacklist--roots ()
  "Validate and expand the configured directory roots without resolving links."
  (unless (and (proper-list-p *tool-path-blacklist*)
               (every (lambda (root) (typep root '(or non-empty-string pathname)))
                      *tool-path-blacklist*))
    (error 'configuration-error
           :message "The tool path blacklist must be a proper list of directory pathnames or nonempty strings."))
  (mapcar (lambda (root)
            (let ((directory
                    (uiop:ensure-directory-pathname
                     (configuration--expanded-working-directory root))))
              (unless (uiop:absolute-pathname-p directory)
                (error 'configuration-error
                       :message "Tool path blacklist directories must be absolute (or begin with ~/)."))
              directory))
          *tool-path-blacklist*))

(-> tool-path-blacklist--within-p (pathname pathname) boolean)
(defun tool-path-blacklist--within-p (path root)
  "Return whether PATH names directory ROOT itself or one of its descendants."
  (and (or (uiop:pathname-equal (uiop:ensure-directory-pathname path) root)
           (uiop:subpathp path root))
       t))

(-> tool-path-blacklist--directory-identity (pathname) t)
(defun tool-path-blacklist--directory-identity (path)
  "Return the identity of existing directory PATH, or NIL for an absent root."
  (let ((status (platform-path-status *platform* path :follow-links-p t)))
    (when (and status (eq (platform-file-status-kind status) ':directory))
      (platform-file-status-identity status))))

(-> tool-path-blacklist--ancestor-identities (pathname) list)
(defun tool-path-blacklist--ancestor-identities (path)
  "Return existing directory identities from PATH's parent through its filesystem root."
  (loop for directory = (uiop:pathname-directory-pathname path)
          then (uiop:pathname-parent-directory-pathname directory)
        for identity = (tool-path-blacklist--directory-identity directory)
        when identity collect identity
        until (<= (length (pathname-directory directory)) 1)))

(-> tool-path-blacklist-check
    (pathname &key (:tool-name non-empty-string) (:recursive-p boolean)) pathname)
(defun tool-path-blacklist-check (path &key (tool-name "resource") recursive-p)
  "Return canonical PATH unless a configured directory forbids it.

Check the requested spelling as well as its current canonical target, including
existing ancestors of missing leaves. Compare directory identities to recognize
filesystem aliases whose native spelling differs. With RECURSIVE-P also refuse
requested trees containing a denied root. Resolution failures propagate."
  (let ((roots (tool-path-blacklist--roots)))
    (labels ((deny (root)
               (error 'tool-path-blacklisted
                      :pathname path :root root :tool-name tool-name
                      :message (format nil "Path ~A is denied by the folder blacklist." path))))
      (dolist (root roots)
        (when (tool-path-blacklist--within-p path root)
          (deny root)))
      (let* ((canonical (canonical-pathname path))
             (ancestors (when roots
                          (tool-path-blacklist--ancestor-identities canonical))))
        (dolist (root roots)
          (let* ((canonical-root (uiop:ensure-directory-pathname (canonical-pathname root)))
                 (identity       (tool-path-blacklist--directory-identity canonical-root)))
            (when (or (tool-path-blacklist--within-p canonical root)
                      (tool-path-blacklist--within-p canonical canonical-root)
                      (tool-path-blacklist--within-p path canonical-root)
                      (and identity (member identity ancestors :test #'equal))
                      (and recursive-p
                           (or (tool-path-blacklist--within-p root (uiop:ensure-directory-pathname path))
                               (tool-path-blacklist--within-p canonical-root
                                                              (uiop:ensure-directory-pathname canonical)))))
              (deny root))))
        canonical))))

(define-condition tool-path-blacklisted (tool-authority-denied)
  ((pathname
    :initarg       :pathname
    :reader        tool-path-blacklisted-pathname
    :type          pathname
    :documentation "The requested path refused by the mandatory folder policy.")
   (root
    :initarg       :root
    :reader        tool-path-blacklisted-root
    :type          pathname
    :documentation "The configured directory responsible for the refusal."))
  (:default-initargs :code ':path-blacklisted)
  (:documentation "A tool path is denied independently of command permissions."))
