(require :asdf)

(let* ((arguments (uiop:command-line-arguments))
       (source-root
         (and (first arguments)
              (uiop:ensure-directory-pathname (first arguments))))
       (assembly
         (and (second arguments)
              (pathname (second arguments))))
       (color-archive
         (and (third arguments)
              (pathname (third arguments)))))
  (labels ((fail (control &rest values)
             "Signal a static runtime build failure using CONTROL and VALUES."
             (error "Autolith static runtime build failed: ~?" control values))

           (run (command &key directory)
             "Run COMMAND in DIRECTORY with visible diagnostics."
             (uiop:run-program command
                               :directory    directory
                               :input        ':interactive
                               :output       ':interactive
                               :error-output ':interactive))

           (load-project ()
             "Load the locked Autolith system and its native libraries."
             (let ((setup (merge-pathnames ".qlot/setup.lisp" source-root)))
               (unless (probe-file setup)
                 (fail "locked dependencies are absent."))
               (load setup)
               (asdf:load-asd (merge-pathnames "autolith-agent.asd" source-root))
               (asdf:load-system :autolith-agent)
               (let ((fff-library (uiop:getenv "AUTOLITH_FFF_LIBRARY")))
                 (unless (and fff-library (probe-file fff-library))
                   (fail "AUTOLITH_FFF_LIBRARY does not name the shared build."))
                 (uiop:symbol-call :cffi :load-foreign-library fff-library))
               (uiop:symbol-call :colorlisp :native-ensure-loaded)))

           (build-sandbox-helpers ()
             "Build the locked cl-exec-sandbox helpers as static executables."
             (let* ((sandbox-root
                      (asdf:system-source-directory :cl-exec-sandbox))
                    (compiler (or (uiop:getenv "CC") "cc"))
                    (helpers
                      '(("helper/cl-exec-sandbox-helper.c"
                         "build/cl-exec-sandbox-helper")
                        ("helper/cl-exec-sandbox-process-group.c"
                         "build/cl-exec-sandbox-process-group"))))
               (loop for (source-name target-name) in helpers
                     for source = (merge-pathnames source-name sandbox-root)
                     for target = (merge-pathnames target-name sandbox-root)
                     do (unless (probe-file source)
                          (fail "the locked sandbox helper source ~A is absent."
                                source-name))
                        (uiop:ensure-all-directories-exist (list target))
                        (run (list compiler "-static" "-std=c11" "-O2"
                                   "-Wall" "-Wextra" "-Werror" "-pedantic"
                                   (namestring source) "-o" (namestring target)))
                     collect target)))

           (build-color-archive ()
             "Build ColorLisp's native sources into COLOR-ARCHIVE."
             (let* ((archive-directory
                      (uiop:pathname-directory-pathname color-archive))
                    (object-directory
                      (merge-pathnames "colorlisp-objects/" archive-directory))
                    (compiler (or (uiop:getenv "CC") "cc"))
                    (sources
                      (uiop:symbol-call
                       :colorlisp :colorlisp--native-source-pathnames))
                    (includes
                      (uiop:symbol-call
                       :colorlisp :colorlisp--native-include-arguments))
                    (objects
                      (loop for index below (length sources)
                            collect (merge-pathnames
                                     (format nil "source-~D.o" index)
                                     object-directory))))
               (uiop:delete-directory-tree object-directory
                                           :validate          t
                                           :if-does-not-exist ':ignore)
               (uiop:ensure-all-directories-exist
                (list object-directory archive-directory))
               (loop for source in sources
                     for object in objects
                     do (run (append (list compiler "-c" "-O2" "-std=gnu11"
                                           "-fvisibility=hidden"
                                           "-o" (namestring object))
                                     includes
                                     (list source))))
               (when (probe-file color-archive)
                 (delete-file color-archive))
               (run (append (list "ar" "rcs" (namestring color-archive))
                            (mapcar #'namestring objects)))
               (uiop:delete-directory-tree object-directory
                                           :validate          t
                                           :if-does-not-exist ':ignore)))

           (linkage-symbol-name (key)
             "Return the foreign symbol name represented by linkage KEY."
             (etypecase key
               (string
                key)
               (cons
                (first key))))

           (assembler-symbol-name-p (name)
             "Return true when NAME is safe as a GNU assembler symbol operand."
             (and (plusp (length name))
                  (every (lambda (character)
                           (or (alphanumericp character)
                               (find character "_.$")))
                         name)))

           (resolved-symbol-names ()
             "Return sorted resolved names and malformed linkage entries."
             (let ((names    '())
                   (rejected '()))
               (maphash
                (lambda (key index)
                  (declare (ignore index))
                  (let ((name (linkage-symbol-name key)))
                    (cond
                      ((not (assembler-symbol-name-p name))
                       (push (format nil "~A (unsupported name)" name)
                             rejected))
                      ((handler-case
                           (sb-sys:find-foreign-symbol-address name)
                         (error () nil))
                       (pushnew name names :test #'string=)))))
                (first sb-sys:*linkage-info*))
               (values (sort names #'string<)
                       (sort rejected #'string<))))

           (write-assembly ()
             "Write ASSEMBLY with names and direct addresses for static lookup."
             (multiple-value-bind (names rejected) (resolved-symbol-names)
               (when rejected
                 (fail "~D foreign linkage entries are unusable: ~{~A~^, ~}"
                       (length rejected) rejected))
               (when (zerop (length names))
                 (fail "no foreign symbols were resolved."))
              (uiop:ensure-all-directories-exist
               (list (uiop:pathname-directory-pathname assembly)))
              (with-open-file (stream assembly
                                      :direction         ':output
                                      :if-exists         ':supersede
                                      :if-does-not-exist ':create
                                      :external-format   ':utf-8)
                (format stream ".section .rodata~%")
                (loop for name in names
                      for index from 0
                      do (format stream ".Lautolith_static_name_~D:~% .asciz ~S~%"
                                 index name))
                (format stream ".section .data.rel.ro,\"aw\"~%")
                (format stream ".balign 8~%.globl autolith_static_symbols~%")
                (format stream ".type autolith_static_symbols, @object~%")
                (format stream "autolith_static_symbols:~%")
                (loop for name in names
                      for index from 0
                      do (format stream " .quad .Lautolith_static_name_~D~% .quad ~A~%"
                                 index name))
                (format stream ".size autolith_static_symbols, .-autolith_static_symbols~%")
                (format stream ".section .rodata~%.balign 8~%")
                (format stream ".globl autolith_static_symbol_count~%")
                (format stream ".type autolith_static_symbol_count, @object~%")
                (format stream "autolith_static_symbol_count:~% .quad ~D~%" (length names))
                (format stream ".size autolith_static_symbol_count, 8~%")
                (format stream ".section .note.GNU-stack,\"\",@progbits~%"))
              (format t "~&Recorded ~D static foreign symbols.~%" (length names)))))
    (handler-case
        (progn
          (unless (and (= (length arguments) 3)
                       source-root assembly color-archive)
            (fail "usage: build-static-release-runtime.lisp SOURCE ASSEMBLY COLOR-ARCHIVE"))
          (load-project)
          (build-sandbox-helpers)
          (build-color-archive)
          (write-assembly))
      (error (condition)
        (format *error-output* "~&~A~%" condition)
        (finish-output *error-output*)
        (uiop:quit 1)))))
