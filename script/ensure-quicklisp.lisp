(require :asdf)
(let ((setup (merge-pathnames "quicklisp/setup.lisp" (user-homedir-pathname))))
  (unless (probe-file setup)
    (let ((installer (merge-pathnames ".quicklisp-installer.lisp"
                                      (uiop:pathname-directory-pathname
                                       (truename *load-truename*)))))
      (unwind-protect
           (progn
             (uiop:run-program
              (list "curl.exe" "--fail" "--location" "--show-error" "--retry" "3"
                    "--output" (uiop:native-namestring installer)
                    "https://beta.quicklisp.org/quicklisp.lisp")
             (load installer)
             (funcall (find-symbol "INSTALL" "QUICKLISP-QUICKSTART")
                      :path (merge-pathnames "quicklisp/" (user-homedir-pathname))))
        (when (probe-file installer)
          (delete-file installer))))))
)
