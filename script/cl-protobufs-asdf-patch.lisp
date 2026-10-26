;;; Packaging replacement for cl-protobufs' ASDF generator operation.
;;; Applied to the dependency source by build-protobufs.lisp.
(in-package #:protobuf-system)

(defmethod perform ((operation proto-to-lisp) (component protobuf-source-file))
  "Generate Lisp with separate argv elements and the privately built plugin."
  (let* ((source-file (first (input-files operation component)))
         (output-file (first (output-files operation component)))
         (plugin-name (if (uiop:os-windows-p)
                          "protoc-gen-cl-pb.exe"
                          "protoc-gen-cl-pb"))
         (private-plugin
           (merge-pathnames
            (concatenate 'string ".autolith-protoc/install/bin/" plugin-name)
            (asdf:system-source-directory :cl-protobufs)))
         (plugin      (or (uiop:getenvp "AUTOLITH_PROTOC_PLUGIN")
                          (and (probe-file private-plugin)
                               (uiop:native-namestring private-plugin))))
         (command
           (append
            (list "protoc")
            (when plugin
              (list (concatenate 'string "--plugin=protoc-gen-cl-pb=" plugin)))
            (mapcar (lambda (path)
                      (concatenate 'string "--proto_path="
                                   (uiop:native-namestring path)))
                    (get-search-paths component))
            (list (format nil "--cl-pb_out=output-file=~A:~A"
                          (file-namestring output-file)
                          (uiop:native-namestring
                           (uiop:pathname-directory-pathname output-file)))
                  (uiop:native-namestring source-file)
                  "--experimental_allow_proto3_optional"))))
    (multiple-value-bind (output error-output status)
        (uiop:run-program command :output             '(:string :stripped t)
                                 :error-output        :output
                                 :ignore-error-status t)
      (declare (ignore error-output))
      (unless (zerop status)
        (error 'protobuf-compile-failed
               :description       (format nil "Failed to compile proto file. Command: ~S Error: ~S"
                                          command output)
               :context-format    "~/asdf-action::format-action/"
               :context-arguments `((,operation . ,component)))))))
