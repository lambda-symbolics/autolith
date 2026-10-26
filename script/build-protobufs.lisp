(require :asdf)

(defun protobuf-build--patch-asdf (root replacement-path)
  "Replace one complete generator method in the installed dependency source."
  (asdf:load-asd (merge-pathnames "cl-protobufs.asdf.asd" root))
  (asdf:load-system :cl-protobufs.asdf)
  (let* ((path (merge-pathnames "asdf.lisp" root))
         (text (uiop:read-file-string path))
         (replacement (uiop:read-file-string replacement-path))
         (package (find-package "PROTOBUF-SYSTEM"))
         (start nil)
         (end nil))
    (let ((*package* package)
          (*read-eval* nil))
      (with-input-from-string (stream text)
        (loop for position = (file-position stream)
              for form = (read stream nil :eof)
              until (eq form :eof)
              when (and (consp form)
                        (eq (first form) 'defmethod)
                        (eq (second form) (find-symbol "PERFORM" "ASDF"))
                        (consp (third form))
                        (equal (first (third form))
                               (list (find-symbol "OPERATION" package)
                                     (find-symbol "PROTO-TO-LISP" package))))
                do (when start
                     (error "More than one protobuf generator method in ~A." path))
                   (setf start position
                         end (file-position stream)))))
    (unless start
      (error "The protobuf generator method was not found in ~A." path))
    ;; Keep dependency comments and all other forms intact.
    (let ((method-start (position #\( replacement
                                  :start (or (search "(defmethod" replacement) 0))))
      (unless method-start
        (error "The protobuf generator replacement has no method."))
      (with-open-file (stream path :direction ':output :if-exists ':supersede)
        (write-string text stream :end start)
        (terpri stream)
        (write-string replacement stream :start method-start)
        (terpri stream)
        (write-string text stream :start end)))
    (load path)))

(defun protobuf-build--plugin (root)
  "Build and install the locked protoc plugin beside its dependency source."
  (let* ((source (merge-pathnames "protoc/" root))
         (build (merge-pathnames ".autolith-protoc/" root))
         (install (merge-pathnames "install/" build)))
    (uiop:run-program
     (list "cmake" "-S" (uiop:native-namestring source)
           "-B" (uiop:native-namestring build)
           "-DCMAKE_CXX_STANDARD=17"
           "-DCMAKE_INCLUDE_CURRENT_DIR=ON"
           (format nil "-DCMAKE_INSTALL_PREFIX=~A"
                   (uiop:native-namestring install)))
     :output *standard-output* :error-output *error-output*)
    (uiop:run-program
     (list "cmake" "--build" (uiop:native-namestring build) "--config" "Release")
     :output *standard-output* :error-output *error-output*)
    (uiop:run-program
     (list "cmake" "--install" (uiop:native-namestring build) "--config" "Release")
     :output *standard-output* :error-output *error-output*)))

(let* ((script-root (uiop:pathname-directory-pathname *load-truename*))
       (arguments (uiop:command-line-arguments))
       (explicit-root (second (member "--dependency-root" arguments :test #'string=)))
       (root (if explicit-root
                 (uiop:ensure-directory-pathname explicit-root)
                 (asdf:system-source-directory :cl-protobufs))))
  (protobuf-build--patch-asdf
   root (merge-pathnames "cl-protobufs-asdf-patch.lisp" script-root))
  (unless (member "--patch-only" arguments :test #'string=)
    (protobuf-build--plugin root)))
