(in-package #:autolith)

;;;; -- Data Transfer Entry Points --

(-> test-data-transfer-commands () null)
(defun test-data-transfer-commands ()
  "Exercise equivalent CLI and in-session scopes without starting a provider."
  (with-test-configuration (configuration root)
    (declare (ignore root))
    (let* ((application (make-instance 'application :configuration configuration))
           (observed    nil)
           (report      (list :pathname "archive.sexp" :workspace nil :workspaces nil
                              :conversations 0 :memories 0 :agendas 0 :plans 0
                              :papercuts 0 :files 0)))
      (labels ((transfer (operation pathname &key workspace received-configuration)
                 (test-assert (eq configuration received-configuration)
                              "both entry points use their explicit configuration")
                 (push (list operation pathname workspace) observed)
                 report)

               (cli (arguments)
                 (let ((*standard-output* (make-string-output-stream))
                       (*error-output*    (make-string-output-stream)))
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
                  (transfer ':export pathname :workspace              workspace
                                              :received-configuration configuration)))
          (list 'data-import
                (lambda (pathname &key workspace configuration)
                  (transfer ':import pathname :workspace              workspace
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
                                       (config :working-directory configuration))
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
                                                                             :type   (pathname-type path))))
                            "command pathnames preserve native backslashes")))
           (dolist (operation '("export" "import"))
             (let ((archive   #P"Mixed Case/archive.sexp")
                   (workspace #P"Mixed Case/workspace/"))
               (setf observed nil)
               (application-operation-call application (format nil "data.~A" operation)
                                           archive :workspace workspace)
               (let* ((command (application-command-find (format nil "/data.~A" operation)))
                      (input   (format nil "~S --workspace ~S" (uiop:native-namestring archive)
                                       (uiop:native-namestring workspace))))
                 (application-command-execute
                  command application (application-client-command-invocation command input)))
               (test-assert (equal (first observed) (second observed))
                            "typed pathname options and structured argument text select the same transfer")))
           (setf observed nil)
           (application-operation-call application "data.export" #P"archive.sexp" :all t)
           (test-assert (null (third (first observed))) "native Boolean :all selects every workspace")
           (dolist (request '(("data.export" "archive.sexp" "--all")
                              ("data.export" "archive.sexp" :all "yes")
                              ("data.export" "archive.sexp" :all t :workspace #P"workspace/")
                              ("data.export" "archive.sexp" :unknown t)
                              ("data.import" "archive.sexp" :all t)
                              ("data.import" "archive.sexp" :workspace)))
             (setf observed nil)
             (test-assert
              (handler-case (progn (apply #'application-operation-call application request) nil)
                (configuration-error () t)
                (program-error () t))
              "invalid native transfer options are rejected")
             (test-assert (null observed) "invalid options cannot start a transfer"))
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

(-> test-data-transfer-receipt-presentation () null)
(defun test-data-transfer-receipt-presentation ()
  "Exercise typed receipts and fullscreen reflow of long, wide native paths."
  (let* ((path   (uiop:native-namestring
                  #P"猫/wide archive/subdirectory/another directory/archive.sexp"))
         (report (list :pathname path :workspace "猫/workspace/"
                       :conversations 11 :memories 12 :agendas 13
                       :plans 14 :papercuts 15 :files 16))
         (entry  (data-transfer-report-entry "Exported" report)))
    (test-assert (terminal-styled-text-p entry) "transfer receipt is a semantic transcript entry")
    (test-assert (command-view-tests--role-p entry ':success "✓ exported") "completed transfer uses success severity")
    (test-assert (command-view-tests--role-p entry ':code path) "archive pathname has code styling")
    (test-assert (command-view-tests--role-p entry ':dim "conversations") "count labels are subordinate metadata")
    (test-assert (command-view-tests--role-p entry ':plain "11") "transfer counts remain visible data")
    (let ((terminal (make-instance 'recording-terminal :columns 90 :rows 12)))
      (with-terminal-ui (ui (terminal-ui-create :terminal terminal :fullscreen-p t))
        (terminal-ui-append-finalized ui ':transfer-receipt entry)
        (dolist (width '(12 37 90))
          (terminal-ui-resize ui width :rows 12)
          (let* ((viewport  (fullscreen-terminal-ui-viewport ui))
                 (texts     (loop for index below (clinedi:transcript-viewport-row-count viewport)
                                  collect (clinedi:transcript-viewport-row-text viewport index)))
                 (flattened (remove #\Space (format nil "~{~A~}" texts))))
            (test-assert (every (lambda (text) (<= (text-cell-width text) width)) texts)
                         "narrow and wide fullscreen rows fit in terminal cells")
            (test-assert (search (remove #\Space path) flattened)
                         "wrapping preserves the complete long Unicode archive pathname"))))))
  nil)
