(require :asdf)

(defun qlot-install--windows-unsafe-pathname-p (pathname)
  "Return true when Windows cannot store PATHNAME because of its characters."
  (let ((namestring (uiop:native-namestring pathname)))
    (loop for character across namestring
          for index from 0
          thereis (or (find character "<>\"|?*")
                      ;; The drive letter's colon is the only legal one.
                      (and (char= character #\:) (/= index 1))))))

(defun qlot-install--skip-tar-entry (size stream)
  "Consume the 512-byte data blocks of a SIZE-byte tar entry from STREAM."
  (let ((block (make-array 512 :element-type '(unsigned-byte 8))))
    (dotimes (index (ceiling size 512))
      (read-sequence block stream))))

(defun qlot-install--tolerate-windows-unsafe-tar-entries ()
  "Make the Quicklisp client skip tar entries whose names Windows rejects.

Some Quicklisp releases carry test fixtures named with characters such as
< (nyaml does), and the client's tarball unpacker aborts the whole release on
the first such entry. On Windows those entries are consumed and reported
instead, so the rest of the release installs; the skipped files are never
loaded by Autolith."
  (let* ((package   (find-package "QL-MINITAR"))
         (save-file (and package (find-symbol "SAVE-FILE" package))))
    (unless (and save-file (fboundp save-file))
      (error "The Quicklisp client does not expose QL-MINITAR::SAVE-FILE."))
    (let ((original (fdefinition save-file)))
      (setf (fdefinition save-file)
            (lambda (file size stream)
              (if (qlot-install--windows-unsafe-pathname-p file)
                  (progn
                    (format *error-output*
                            "~&Skipping ~A: Windows cannot store that file name.~%"
                            (uiop:native-namestring file))
                    (qlot-install--skip-tar-entry size stream))
                  (funcall original file size stream)))))))

(defun qlot-install--use-windows-directory-cleanup (script-directory)
  "Remove Qlot temporary checkouts without following Windows directory links."
  (let ((cleanup (find-symbol "DELETE-TMP-DIRECTORY" "QLOT/UTILS/TMP"))
        (script  (merge-pathnames "qlot-delete-directory.ps1" script-directory)))
    (unless (and cleanup (fboundp cleanup))
      (error "The loaded Qlot does not expose temporary directory cleanup."))
    (setf (fdefinition cleanup)
          (lambda (directory)
            (uiop:run-program
             (list "powershell.exe" "-NoProfile" "-NonInteractive"
                   "-ExecutionPolicy" "Bypass" "-File" (uiop:native-namestring script)
                   "-Path" (uiop:native-namestring directory))
             :output *standard-output* :error-output *error-output*)))))

(let* ((script-path      (truename *load-truename*))
       (script-directory (uiop:pathname-directory-pathname script-path))
       (source-root      (uiop:pathname-parent-directory-pathname script-directory))
       (arguments        (uiop:command-line-arguments))
       (updates          (rest (member "--update" arguments :test #'string=)))
       (project-root     (let ((argument (and (not (equal (first arguments) "--update"))
                                              (first arguments))))
                           (if argument
                               (uiop:merge-pathnames*
                                (uiop:ensure-directory-pathname argument) (uiop:getcwd))
                               source-root)))
       (quicklisp-setup  (merge-pathnames "quicklisp/setup.lisp"
                                          (user-homedir-pathname))))
  (unless (probe-file quicklisp-setup)
    (error "Autolith bootstrap needs Quicklisp at ~A" quicklisp-setup))
  (load quicklisp-setup)
  (uiop:symbol-call '#:ql '#:quickload :cffi :silent t)
  (let ((profile-library-directory
          (merge-pathnames ".guix-profile/lib/" (user-homedir-pathname)))
        (library-directories
          (find-symbol "*FOREIGN-LIBRARY-DIRECTORIES*" "CFFI")))
    (when (probe-file profile-library-directory)
      (pushnew profile-library-directory
               (symbol-value library-directories)
               :test #'equal)))
  (uiop:symbol-call '#:ql '#:quickload :qlot :silent t)
  (when (uiop:os-windows-p)
    (qlot-install--tolerate-windows-unsafe-tar-entries)
    (qlot-install--use-windows-directory-cleanup script-directory)
    (let ((exclusions (find-symbol "*EXCLUDE-DIRECTORIES*" "QLOT/UTILS/ASDF")))
      (unless (and exclusions (boundp exclusions))
        (error "The loaded Qlot does not expose its metadata directory exclusions."))
      (pushnew "test-fixtures" (symbol-value exclusions) :test #'equal)))
  (let ((qlot-project-root (find-symbol "*PROJECT-ROOT*" "QLOT")))
    (unless qlot-project-root
      (error "The loaded Qlot does not expose its project root."))
    (progv (list qlot-project-root) (list project-root)
      (uiop:with-current-directory (project-root)
        (cond
          (updates
           (uiop:symbol-call '#:qlot '#:update updates))
          ((member :bsd *features*)
           (uiop:symbol-call '#:qlot '#:install :jobs 1))
          (t
           (uiop:symbol-call '#:qlot '#:install)))))))
