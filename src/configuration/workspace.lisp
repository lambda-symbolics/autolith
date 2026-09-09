(in-package #:autolith)

;;;; -- Workspace Identity --

(defparameter *workspace-project-depth-limit* 64
  "The most ancestor directories inspected for a project root.")

(defparameter *workspace-directory-identifier-key*
  (make-array 16
              :element-type '(unsigned-byte 8)
              :initial-contents
              '(65 117 116 111 108 105 116 104 87 111 114 107 115 112 97 99))
  "The public fixed SipHash key used for stable workspace state identifiers.")

(-> workspace-directory-name ((or pathname string)) string)
(defun workspace-directory-name (directory)
  "Return DIRECTORY as a canonical absolute directory namestring."
  (namestring
   (uiop:ensure-directory-pathname
    (platform-truename *platform* (uiop:ensure-directory-pathname directory)))))

(-> workspace-directory-identifier ((or pathname string)) string)
(defun workspace-directory-identifier (directory)
  "Return a compact deterministic identifier for canonical DIRECTORY."
  (let ((mac
          (make-mac ':siphash
                    *workspace-directory-identifier-key*
                    :digest-length 16)))
    (update-mac
     mac
     (sb-ext:string-to-octets (workspace-directory-name directory)
                              :external-format ':utf-8))
    (let ((digest (produce-mac mac)))
      (with-output-to-string (stream)
        (loop for octet across digest
              do (format stream "~2,'0X" octet))))))

(-> workspace-project-root (pathname) pathname)
(defun workspace-project-root (working-directory)
  "Return the nearest ancestor holding a .git marker, or WORKING-DIRECTORY.

The walk never continues above the nearest project root. A workspace without a
Git marker is its own project identity."
  (labels ((marker-p (directory)
             "Return true when DIRECTORY contains a .git entry."
             (and (or (uiop:directory-exists-p
                       (merge-pathnames ".git/" directory))
                      (uiop:file-exists-p (merge-pathnames ".git" directory)))
                  t)))
    (loop repeat *workspace-project-depth-limit*
          for directory = working-directory
            then (uiop:pathname-parent-directory-pathname directory)
          for parent = (uiop:pathname-parent-directory-pathname directory)
          when (marker-p directory)
            return directory
          when (equal directory parent)
            return working-directory
          finally (return working-directory))))

(-> workspace-autolith-notes-path (pathname) pathname)
(defun workspace-autolith-notes-path (working-directory)
  "Return the root AUTOLITH.org pathname for WORKING-DIRECTORY."
  (merge-pathnames "AUTOLITH.org"
                   (workspace-project-root working-directory)))
