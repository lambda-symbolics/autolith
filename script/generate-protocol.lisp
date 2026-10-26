(require :asdf)

(let* ((script-root (uiop:pathname-directory-pathname *load-truename*))
       (root (uiop:pathname-parent-directory-pathname script-root))
       (setup (merge-pathnames ".qlot/setup.lisp" root)))
  (load setup)
  (let* ((dependency-root (asdf:system-source-directory :cl-protobufs))
         (plugin-name (if (uiop:os-windows-p)
                          "protoc-gen-cl-pb.exe"
                          "protoc-gen-cl-pb"))
         (private-plugin
           (merge-pathnames
            (concatenate 'string ".autolith-protoc/install/bin/" plugin-name)
            dependency-root))
         (plugin (or (uiop:getenvp "AUTOLITH_PROTOC_PLUGIN")
                     (and (probe-file private-plugin)
                          (uiop:native-namestring private-plugin)))))
    (dolist (spec '(("autolith/common/v1/value.proto" "value.lisp")
                    ("autolith/parent/v1/protocol.proto" "protocol.lisp")))
      (uiop:run-program
       (append
        (list "protoc")
        (when plugin
          (list (concatenate 'string "--plugin=protoc-gen-cl-pb=" plugin)))
        (list (concatenate 'string "--proto_path="
                           (uiop:native-namestring (merge-pathnames "proto/" root)))
              (format nil "--cl-pb_out=output-file=~A:~A"
                      (second spec)
                      (uiop:native-namestring
                       (merge-pathnames "proto/generated/" root)))
              (uiop:native-namestring
               (merge-pathnames (first spec) (merge-pathnames "proto/" root)))))
       :output *standard-output* :error-output *error-output*))))
