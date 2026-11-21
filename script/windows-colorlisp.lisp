(require :asdf)
(load (merge-pathnames "roots.lisp" (uiop:pathname-directory-pathname *load-truename*)))

;;;; -- Windows ColorLisp Release Library --

(let* ((arguments (uiop:command-line-arguments))
       (action    (first arguments))
       (target    (and (second arguments)
                       (uiop:parse-native-namestring (second arguments))))
       (source    (uiop:pathname-parent-directory-pathname
                   (uiop:pathname-directory-pathname *load-truename*))))
  (unless (and (= (length arguments) 2)
               (member action '("copy" "validate") :test #'string=))
    (error "Usage: windows-colorlisp.lisp copy|validate DLL"))
  (load (merge-pathnames ".qlot/setup.lisp" source))
  (uiop:symbol-call '#:ql '#:quickload :colorlisp :silent t)
  (when (string= action "copy")
    (let ((library (uiop:symbol-call '#:colorlisp '#:native-library-path)))
      (uiop:symbol-call '#:colorlisp '#:native-probe library :windows-image-p t)
      (ensure-directories-exist target)
      (uiop:copy-file library target)))
  (uiop:symbol-call '#:colorlisp '#:native-probe target :windows-image-p t)
  (format t "~&Packaged ColorLisp highlighting passed: ~A~%" target))
