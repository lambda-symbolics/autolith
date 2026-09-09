(in-package #:autolith)

;;;; -- Data Transfer Entry Points --

(-> test-data-transfer-commands () null)
(defun test-data-transfer-commands ()
  "Exercise equivalent CLI and in-session scopes without starting a provider."
  (with-test-configuration (configuration root)
    (declare (ignore root))
    (let* ((application (make-instance 'application :configuration configuration))
           (observed nil)
           (report (list :pathname "archive.sexp" :workspace nil :workspaces nil
                         :conversations 0 :memories 0 :agendas 0 :plans 0
                         :papercuts 0 :files 0)))
      (labels ((transfer (operation pathname &key workspace received-configuration)
                 (test-assert (eq configuration received-configuration)
                              "both entry points use their explicit configuration")
                 (push (list operation pathname workspace) observed)
                 report)

               (cli (arguments)
                 (let ((*standard-output* (make-string-output-stream))
                       (*error-output* (make-string-output-stream)))
                   (catch 'data-command-exit
                     (main arguments)
                     0))))
        (test-call-with-function-replacements
         (list
          (list 'configuration-create
                (lambda (&rest options)
                  (test-assert (getf options :defer-provider-validation-p)
                               "data CLI does not require provider authentication")
                  configuration))
          (list 'data-export
                (lambda (pathname &key workspace configuration)
                  (transfer ':export pathname :workspace workspace
                                              :received-configuration configuration)))
          (list 'data-import
                (lambda (pathname &key workspace configuration)
                  (transfer ':import pathname :workspace workspace
                                              :received-configuration configuration)))
          (list 'application-present
                (lambda (&rest arguments) (declare (ignore arguments)) nil))
          (list 'main--start-session
                (lambda (&rest arguments)
                  (declare (ignore arguments))
                  (error "A data command tried to start a session.")))
          (list 'worker-main
                (lambda () (error "A data operand selected the internal worker.")))
          (list 'uiop:quit
                (lambda (status &key urgent)
                  (declare (ignore urgent))
                  (throw 'data-command-exit status))))
         (lambda ()
           (dolist (operation '("export" "import"))
             (dolist (workspace '(nil "two words" "--recovery" "back\\slash*" "new [repo]"))
               (setf observed nil)
               (test-assert
                (zerop (cli (append (list "data" operation "archive.sexp")
                                    (when workspace (list "--workspace" workspace)))))
                "valid data CLI arguments succeed")
               (apply #'application-operation-call
                      application (format nil "data.~A" operation) "archive.sexp"
                      (when workspace (list :workspace workspace)))
               (test-assert (equal (first observed) (second observed))
                            "CLI and session commands select the same operation and paths")
               (when workspace
                 (test-assert
                  (equal (concatenate 'string
                                      (uiop:native-namestring
                                       (configuration-working-directory configuration))
                                      workspace)
                         (uiop:native-namestring (third (first observed))))
                  "both interfaces pass the requested native workspace pathname"))))
           (setf observed nil)
           (test-assert (zerop (cli '("data" "export" "archive.sexp" "--all")))
                        "all-workspace export is explicit when desired")
           (test-assert (null (third (first observed))) "all selects no workspace filter")
           (test-assert (zerop (cli '("data" "export" "--" "--worker")))
                        "an archive operand cannot select an internal worker")
           (with-test-fixture (':wildcard-file-names
                               "command pathnames holding metacharacters")
             (let* ((name "literal\\name*.sexp")
                    (path (data-transfer--command-path name configuration)))
               (test-assert (not (wild-pathname-p path))
                            "command pathnames preserve literal wildcard characters")
               (test-assert (equal name (uiop:native-namestring (make-pathname :name (pathname-name path)
                                                                             :type (pathname-type path))))
                            "command pathnames preserve native backslashes")))
           (dolist (failure '((data-transfer-error :reason :invalid
                                                  :message "Invalid test archive.")
                              (file-error)))
             (test-call-with-function-replacements
              (list (list 'data-import
                          (lambda (pathname &key workspace configuration)
                            (declare (ignore workspace configuration))
                            (apply #'error (first failure) :pathname pathname (rest failure)))))
              (lambda ()
                (test-assert (= 1 (cli '("data" "import" "broken.sexp")))
                             "archive and filesystem failures use an operational exit status"))))
           (dolist (arguments '(("data") ("data" "export") ("data" "import")
                                ("data" "export" "file" "extra")
                                ("data" "import" "file" "--unknown")
                                ("data" "import" "file" "--all")
                                ("data" "export" "file" "--all" "--workspace" ".")
                                ("data" "export" "file" "--workspace")))
             (setf observed nil)
             (test-assert (= 64 (cli arguments)) "invalid data syntax exits 64")
             (test-assert (null observed) "invalid syntax never invokes a transfer"))
           (dolist (arguments '(("data" "--help") ("data" "export" "--help")
                                ("data" "import" "--help")))
             (setf observed nil)
             (test-assert (zerop (cli arguments)) "data commands expose help")
             (test-assert (null observed) "help performs no transfer")))))))
  nil)
