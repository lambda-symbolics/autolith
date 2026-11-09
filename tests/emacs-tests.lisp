(in-package #:autolith)

;;;; -- Emacs Test Support --

(-> emacs-test-configuration (pathname (option pathname)) configuration)
(defun emacs-test-configuration (root socket)
  "Return a configuration working in ROOT that names Emacs server SOCKET."
  (configuration-create
   :source-root         (asdf:system-source-directory :autolith)
   :working-directory   root
   :emacs-server-socket socket))

(-> emacs-test-context (configuration &key (:authorization keyword)) tool-context)
(defun emacs-test-context (configuration &key (authorization ':full-access))
  "Return a tool context with the resource and emacs.* tools whose command authorization answers AUTHORIZATION."
  (let ((registry (make-instance 'tool-registry)))
    (default-tools--register-workspace registry)
    (emacs-register-tools registry)
    (make-instance 'tool-context
                   :configuration configuration
                   :registry      registry
                   :worker        nil
                   :conversation  (conversation-create configuration :identifier "emacs")
                   :command-authorization-function
                   (lambda (command directory)
                     (declare (ignore command directory))
                     authorization))))

(-> emacs-test-invoke (tool-context string string &key (:arguments hash-table)) tool-result)
(defun emacs-test-invoke (context namespace name &key arguments)
  "Invoke NAMESPACE.NAME through ordinary lookup, validation and error projection."
  (tool-registry-execute-call (tool-context-registry context)
                              (json-object "namespace" namespace "name" name
                                           "arguments" (json-encode arguments))
                              context))

(-> emacs-test-call (tool-context string hash-table) json-object)
(defun emacs-test-call (context name arguments)
  "Invoke emacs.NAME, assert success and decode its JSON result."
  (let ((result (emacs-test-invoke context "emacs" name :arguments arguments)))
    (test-assert (tool-result-success-p result)
                 (format nil "emacs.~A succeeds: ~A" name (tool-result-content result)))
    (json-decode (tool-result-content result))))

(-> emacs-test-read (tool-context string) string)
(defun emacs-test-read (context uri)
  "Read URI with resource.read, assert success and return the result text."
  (let ((result (emacs-test-invoke context "resource" "read" :arguments (json-object "uri" uri))))
    (test-assert (tool-result-success-p result)
                 (format nil "resource.read ~A succeeds: ~A" uri (tool-result-content result)))
    (tool-result-content result)))

(-> emacs-test-revision (string) string)
(defun emacs-test-revision (text)
  "Return the Revision: line's value in resource result TEXT."
  (let* ((start (+ (search "Revision: " text) (length "Revision: ")))
         (end   (position #\Newline text :start start)))
    (subseq text start end)))

(-> emacs-test-replace-line
    (tool-context string &key (:revision string) (:line (integer 1)) (:content string))
    tool-result)
(defun emacs-test-replace-line (context uri &key revision line content)
  "Replace LINE of URI observed at REVISION with CONTENT through resource.edit."
  (emacs-test-invoke context "resource" "edit"
                     :arguments (json-object "uri" uri
                                             "base-revision" revision
                                             "operations" (vector (json-object "op" "replace-lines"
                                                                               "start-line" line
                                                                               "end-line" line
                                                                               "content" content)))))

(-> emacs-test-serve-once (pathname function) t)
(defun emacs-test-serve-once (socket function)
  "Listen on SOCKET and answer one connection by calling FUNCTION with its request line and stream."
  (let ((listener (platform-local-listener *platform* socket)))
    (make-thread
     (lambda ()
       (unwind-protect
            (let* ((connection (sb-bsd-sockets:socket-accept listener))
                   (stream     (sb-bsd-sockets:socket-make-stream
                                connection :input t :output t :element-type 'character
                                           :external-format ':utf-8 :buffering ':full)))
              (unwind-protect
                   (progn
                     (funcall function (read-line stream nil "") stream)
                     (finish-output stream))
                (sb-bsd-sockets:socket-close connection)))
         (sb-bsd-sockets:socket-close listener)
         (platform-delete-file *platform* socket)))
     :name "Emacs test server")))

(-> emacs-test-short-directory () pathname)
(defun emacs-test-short-directory ()
  "Return a fresh short temporary directory; Unix socket paths are limited in length."
  (let ((directory (merge-pathnames (format nil "aet-~D-~D/" (current-process-id) (random 100000))
                                    (uiop:temporary-directory))))
    (ensure-directories-exist directory)
    directory))

(-> emacs-test-await-exit (integer real) boolean)
(defun emacs-test-await-exit (process-id timeout-seconds)
  "Wait up to TIMEOUT-SECONDS for PROCESS-ID to exit; return T when it has."
  (let ((deadline (+ (get-internal-real-time)
                     (* timeout-seconds internal-time-units-per-second))))
    (loop
      (unless (platform-process-alive-p *platform* process-id)
        (return t))
      (when (>= (get-internal-real-time) deadline)
        (return nil))
      (sleep 0.05))))

(-> emacs-test-executable () (option pathname))
(defun emacs-test-executable ()
  "Return the Emacs executable on the native PATH, or NIL."
  (loop for directory in (uiop:getenv-pathnames "PATH" :ensure-directory t)
        when directory
          do (loop for name in '("emacs" "emacs.exe")
                   for candidate = (merge-pathnames name directory)
                   when (platform-executable-file-p *platform* candidate)
                     do (return-from emacs-test-executable candidate))))

(-> test-emacs-executable-discovery () null)
(defun test-emacs-executable-discovery ()
  "Resolve native PATH entries, spaces, empty entries and Windows executable names."
  (with-test-configuration (configuration root)
    (declare (ignore configuration))
    (let* ((first             (merge-pathnames "Program Files/Emacs/bin/" root))
           (second            (merge-pathnames "later/bin/" root))
           (missing           (merge-pathnames "missing/" root))
           (first-executable  (merge-pathnames "emacs.exe" first))
           (second-executable (merge-pathnames "emacs.exe" second))
           (separator         (uiop:inter-directory-separator)))
      (dolist (executable (list first-executable second-executable))
        (ensure-directories-exist executable)
        (publish-file executable "Emacs discovery fixture")
        (platform-copy-file-permissions *platform* sb-ext:*runtime-pathname* executable))
      (with-test-environment (("PATH" ""))
        (test-assert (null (emacs-test-executable))
                     "An empty PATH has no Emacs executable."))
      (with-test-environment (("PATH" (uiop:native-namestring missing)))
        (test-assert (null (emacs-test-executable))
                     "An absent PATH directory has no Emacs executable."))
      (with-test-environment
          (("PATH" (format nil "~C~A~C~A~C~C~A~C"
                           separator (uiop:native-namestring missing)
                           separator (uiop:native-namestring first)
                           separator separator (uiop:native-namestring second)
                           separator)))
        (test-assert (equal (emacs-test-executable) first-executable)
                     "Native separators preserve drive letters and paths with spaces.")
        (delete-file first-executable)
        (test-assert (equal (emacs-test-executable) second-executable)
                     "Lookup continues to the next PATH directory.")))))


;;;; -- Protocol --

(-> test-emacs-server-quoting () null)
(defun test-emacs-server-quoting ()
  "Arguments are quoted exactly as server.el quotes them and round-trip."
  (dolist (case (list (list "plain" "plain")
                      (list "two words" "two&_words")
                      (list "-flag" "&-flag")
                      (list "a&b" "a&&b")
                      (list (format nil "line~%line") "line&nline")
                      (list "(insert \"λ - x\")" "(insert&_\"λ&_&-&_x\")")))
    (destructuring-bind (text quoted) case
      (test-assert (equal (emacs-server-quote text) quoted)
                   (format nil "~S quotes as ~S" text quoted))
      (test-assert (equal (emacs-server-unquote quoted) text)
                   (format nil "~S unquotes to ~S" quoted text))))
  nil)

(-> test-emacs-server-fake-replies () null)
(defun test-emacs-server-fake-replies ()
  "Printed chunks join, -error becomes an elisp error and slow servers time out."
  (with-platform-capability (':local-sockets "Emacs server protocol")
    (let ((directory (emacs-test-short-directory)))
      (unwind-protect
           (let ((socket  (merge-pathnames "server" directory))
                 (request nil))
             (join-thread
              (prog1 (emacs-test-serve-once
                      socket
                      (lambda (line stream)
                        (setf request line)
                        (format stream "-emacs-pid 42~%-print \"abc~%-print-nonl &_def\"~%")))
                (test-assert (equal (emacs-server-evaluate socket "(foo bar)") "\"abc def\"")
                             "Print chunks join and unquote.")))
             (test-assert (equal request "-eval (foo&_bar) ")
                          "The request is one quoted -eval line ending in a space.")
             (join-thread
              (prog1 (emacs-test-serve-once
                      socket
                      (lambda (line stream)
                        (declare (ignore line))
                        (format stream "-error Symbol&-s&_value&_is&_void~%")))
                (handler-case (progn (emacs-server-evaluate socket "x")
                                     (test-assert nil "An -error reply signals."))
                  (emacs-server-error (condition)
                    (test-assert (eq (emacs-server-error-reason condition) ':elisp-error)
                                 "An -error reply is an elisp error.")
                    (test-assert (search "Symbol-s value is void" (autolith-error-message condition))
                                 "The elisp error message is unquoted.")))))
             (let ((encoded (usb8-array-to-base64-string (utf8-string-to-octets "{\"λ\":1}"))))
               (join-thread
                (prog1 (emacs-test-serve-once
                        socket
                        (lambda (line stream)
                          (declare (ignore line))
                          (format stream "-print \"~A\"~%" encoded)))
                  (test-assert (eql (json-get (emacs-server-call
                                               (emacs-test-configuration directory socket) "t")
                                              "λ")
                                    1)
                               "A base64 JSON value decodes to an object."))))
             (let ((thread (emacs-test-serve-once
                            socket
                            (lambda (line stream)
                              (declare (ignore line stream))
                              (sleep 0.8)))))
               (handler-case (progn (emacs-server-evaluate socket "x" :timeout 0.3)
                                    (test-assert nil "A silent server times out."))
                 (emacs-server-error (condition)
                   (test-assert (eq (emacs-server-error-reason condition) ':timeout)
                                "A silent server is a timeout.")))
               (join-thread thread)))
        (platform-delete-directory-tree *platform* directory :validate t :if-does-not-exist ':ignore))))
  nil)


;;;; -- Registration --

(-> test-emacs-tools-registration () null)
(defun test-emacs-tools-registration ()
  "The emacs.* tools and emacs: scheme exist only while a configured Emacs server socket exists."
  (with-test-configuration (base root)
    (declare (ignore base))
    (flet ((registered-p (configuration)
             (let ((registry (make-default-tool-registry :configuration configuration)))
               (unwind-protect
                    (let ((tool-p   (and (tool-registry-find registry "emacs" "visit") t))
                          (scheme-p (and (search "emacs:current"
                                                 (tool-description
                                                  (tool-registry-find registry "resource" "read")))
                                         t)))
                      (test-assert (eq tool-p scheme-p)
                                   "The emacs.* tools and the emacs: scheme appear together.")
                      tool-p)
                 (tool-registry-close-runtime-state registry)))))
      (test-assert (not (registered-p (emacs-test-configuration root nil)))
                   "Without a socket setting nothing Emacs is registered.")
      (test-assert (not (registered-p (emacs-test-configuration
                                       root (merge-pathnames "missing/server" root))))
                   "A named socket that does not exist registers nothing.")
      (with-platform-capability (':local-sockets "Emacs tool registration")
        (let* ((directory (emacs-test-short-directory))
               (socket    (merge-pathnames "server" directory))
               (listener  (platform-local-listener *platform* socket)))
          (unwind-protect
               (test-assert (registered-p (emacs-test-configuration root socket))
                            "An existing Emacs server socket registers the tools and scheme.")
            (sb-bsd-sockets:socket-close listener)
            (platform-delete-directory-tree *platform* directory :validate t :if-does-not-exist ':ignore))))))
  nil)


;;;; -- Real Emacs --

(-> emacs-test-resource-authority (pathname configuration &key (:file pathname) (:outside pathname)) null)
(defun emacs-test-resource-authority (socket configuration &key file outside)
  "Exercise filtered snapshots, region authorization and retargeted edits in real Emacs."
  (let* ((sandbox      (emacs-test-context configuration :authorization ':sandboxed))
         (full         (emacs-test-context configuration))
         (outside-name (file-namestring outside))
         (outside-uri  (emacs-buffer-uri outside-name)))
    (emacs-server-evaluate
     socket
     (format nil "(progn (make-frame-visible (selected-frame)) (switch-to-buffer ~S) (auto-save-mode -1) (erase-buffer) (insert \"OUTSIDE-REGION-SECRET\") (goto-char (point-min)) (set-mark (point-max)) (setq transient-mark-mode t mark-active t autolith-secret-reads 0) (advice-add 'buffer-substring-no-properties :before (lambda (&rest ignored) (when (equal (buffer-name) ~S) (setq autolith-secret-reads (1+ autolith-secret-reads))))))"
             outside-name outside-name))
    (let ((text (emacs-test-read sandbox "emacs:current")))
      (test-assert (search "notes.lisp" text) "Workspace buffers are listed without full access.")
      (test-assert (not (search outside-name text)) "Outside buffer names are filtered.")
      (test-assert (not (search (uiop:native-namestring outside) text)) "Outside paths are filtered.")
      (test-assert (not (search "OUTSIDE-REGION-SECRET" text)) "An unauthorized selected region is filtered."))
    (test-assert (not (tool-result-success-p
                       (emacs-test-invoke sandbox "resource" "read"
                                          :arguments (json-object "uri" outside-uri))))
                 "Outside buffer reads require full access.")
    (test-assert (equal "0" (emacs-server-evaluate socket "autolith-secret-reads"))
                 "Denied region and buffer reads never retrieve outside text.")
    (let ((text (emacs-test-read full "emacs:current")))
      (test-assert (search (uiop:native-namestring outside) text) "Full access includes outside paths.")
      (test-assert (search "OUTSIDE-REGION-SECRET" text) "Full access includes the selected outside region."))
    (emacs-server-evaluate
     socket "(progn (switch-to-buffer \"*autolith notes*\") (goto-char (point-min)) (set-mark (point-max)) (setq mark-active t))")
    (let ((text (emacs-test-read sandbox "emacs:current")))
      (test-assert (not (search "autolith%20notes" text)) "Non-file window names require full access.")
      (test-assert (not (search "scratch" text)) "Non-file selected regions require full access."))
    (test-assert (not (tool-result-success-p
                       (emacs-test-invoke sandbox "resource" "read"
                                          :arguments (json-object "uri" (emacs-buffer-uri "*autolith notes*")))))
                 "Non-file buffer reads require full access.")
    (test-assert (search "scratch" (emacs-test-read full "emacs:current"))
                 "Full access includes non-file selected regions.")
    (emacs-server-evaluate
     socket "(progn (switch-to-buffer \"notes.lisp\") (goto-char (point-min)) (set-mark (point-max)) (setq mark-active t))")
    (test-assert (search "λ unsaved" (emacs-test-read sandbox "emacs:current"))
                 "Workspace selected regions are usable without full access.")
    (let* ((text     (emacs-test-read sandbox "emacs:buffer/notes.lisp"))
           (revision (emacs-test-revision text)))
      (emacs-server-evaluate
       socket (format nil "(with-current-buffer \"notes.lisp\" (setq buffer-file-name ~S))"
                      (uiop:native-namestring outside)))
      (let ((result (emacs-test-replace-line sandbox "emacs:buffer/notes.lisp"
                                             :revision revision
                                             :line     1
                                             :content  "unauthorized edit")))
        (test-assert (not (tool-result-success-p result)) "Retargeting a buffer invalidates its edit authority.")
        (test-assert (not (search "unauthorized edit"
                                  (emacs-server-evaluate socket "(with-current-buffer \"notes.lisp\" (buffer-string))")))
                     "A retargeted edit does not modify the buffer."))
      (emacs-server-evaluate
       socket (format nil "(with-current-buffer \"notes.lisp\" (setq buffer-file-name ~S))"
                      (uiop:native-namestring file)))))
  nil)

(-> test-emacs-tools-real-daemon () null)
(defun test-emacs-tools-real-daemon ()
  "emacs: resources and emacs.* tools work against a real Emacs daemon."
  (let ((emacs (emacs-test-executable)))
    (if (or (null emacs) (not (platform-supports-p *platform* ':local-sockets)))
        (test-withheld ':emacs "real Emacs server tools")
        (with-test-configuration (base root)
          (declare (ignore base))
          (let* ((runtime           (emacs-test-short-directory))
                 (name              "autolith-test")
                 (socket            (merge-pathnames (format nil "emacs/~A" name) runtime))
                 (file              (merge-pathnames "notes.lisp" root))
                 (outside           (merge-pathnames "outside.txt" runtime))
                 (configuration     (emacs-test-configuration root socket))
                 (daemon-process-id nil))
            (publish-file file (format nil "(defun greet ()~%  \"hello\")~%"))
            (publish-file outside (format nil "outside~%"))
            (unwind-protect
                 (progn
                   (uiop:run-program (list "env" (format nil "XDG_RUNTIME_DIR=~A"
                                                         (uiop:native-namestring runtime))
                                           (uiop:native-namestring emacs) "-Q"
                                           (format nil "--daemon=~A" name))
                                     :output nil :error-output nil)
                   (test-assert (probe-file socket) "The Emacs daemon listens on its socket.")
                   (setf daemon-process-id
                         (parse-integer (emacs-server-evaluate socket "(emacs-pid)")))
                   (emacs-server-evaluate
                    socket
                    (format nil "(progn (with-current-buffer (find-file-noselect ~S) (goto-char (point-max)) (insert \"; λ unsaved\\n\")) (find-file-noselect ~S) (with-current-buffer (get-buffer-create \"*autolith notes*\") (insert \"scratch\")))"
                            (uiop:native-namestring file) (uiop:native-namestring outside)))
                   (emacs-test-resource-authority socket configuration :file file :outside outside)
                   (let ((context (emacs-test-context configuration))
                         (uri     "emacs:buffer/notes.lisp"))
                     (test-assert (search "- emacs:buffer/notes.lisp, visiting"
                                          (emacs-test-read context "emacs:current"))
                                  "emacs:current lists file buffers by their URI.")
                     (let ((text (emacs-test-read context uri)))
                       (test-assert (search "λ unsaved" text)
                                    "A buffer read sees unsaved unicode edits.")
                       (test-assert (search "Unsaved changes: yes" text)
                                    "A buffer read reports the unsaved state.")
                       (test-assert (search "Visible lines: 1-3 of 3" text)
                                    "A buffer read counts the live lines.")
                       (let* ((edited (emacs-test-replace-line context uri
                                                               :revision (emacs-test-revision text)
                                                               :line     1
                                                               :content  "(defun greet (name)"))
                              (buffer (emacs-server-evaluate
                                       socket "(with-current-buffer \"notes.lisp\" (buffer-string))")))
                         (test-assert (tool-result-success-p edited)
                                      (format nil "A buffer edit applies: ~A" (tool-result-content edited)))
                         (test-assert (search "(defun greet (name)" buffer)
                                      "The edit lands in the live buffer.")
                         (test-assert (search "λ unsaved" buffer)
                                      "The edit keeps the person's unsaved text.")
                         (test-assert (not (search "(name)" (uiop:read-file-string file)))
                                      "The edit leaves the file on disk unsaved.")
                         (emacs-server-evaluate
                          socket "(with-current-buffer \"notes.lisp\" (goto-char (point-max)) (insert \";; typed\\n\"))")
                         (let ((stale (emacs-test-replace-line
                                       context uri
                                       :revision (emacs-test-revision (tool-result-content edited))
                                       :line     1
                                       :content  "(defun greet ()")))
                           (test-assert (and (not (tool-result-success-p stale))
                                             (search "stale" (tool-result-content stale)))
                                        "An edit after the person typed is stale.")
                           (test-assert (search "(defun greet (name)"
                                                (emacs-server-evaluate
                                                 socket "(with-current-buffer \"notes.lisp\" (buffer-string))"))
                                        "A stale edit leaves the buffer alone."))))
                     (test-assert (search "scratch" (emacs-test-read
                                                     context (emacs-buffer-uri "*autolith notes*")))
                                  "Buffers without files resolve by their encoded name.")
                     (test-assert (not (tool-result-success-p
                                        (emacs-test-invoke context "resource" "read"
                                                           :arguments (json-object "uri" "emacs:buffer/absent"))))
                                  "A missing buffer fails to read.")
                     (test-assert (equal (json-get (emacs-test-call context "visit"
                                                                    (json-object "path" "notes.lisp"
                                                                                 "line" 2))
                                                   "buffer")
                                         "notes.lisp")
                                  "emacs.visit shows the file.")
                     (test-assert (equal (json-get (emacs-test-call context "eval"
                                                                    (json-object "form" "(+ 1 2)"))
                                                   "value")
                                         "3")
                                  "emacs.eval returns the printed value."))
                   (let ((denied (emacs-test-context configuration :authorization ':sandboxed)))
                     (test-assert (not (tool-result-success-p
                                        (emacs-test-invoke denied "resource" "read"
                                                           :arguments (json-object "uri" "emacs:buffer/outside.txt"))))
                                  "A buffer visiting a file outside the roots needs full access.")
                     (test-assert (not (tool-result-success-p
                                        (emacs-test-invoke denied "emacs" "eval"
                                                           :arguments (json-object "form" "(setq autolith-test-ran t)"))))
                                  "emacs.eval without full access fails.")
                     (let ((ran (emacs-server-evaluate socket "(boundp 'autolith-test-ran)")))
                       (test-assert (equal ran "nil")
                                    (format nil "A denied form never reaches Emacs: ~S" ran)))))
              (ignore-errors (emacs-server-evaluate socket "(kill-emacs)" :timeout 2))
              (when (and daemon-process-id
                         (not (emacs-test-await-exit daemon-process-id 10)))
                (error 'emacs-server-error
                       :message "The test Emacs daemon did not exit before cleanup."
                       :reason  ':timeout))
              (platform-delete-directory-tree *platform* runtime :validate t :if-does-not-exist ':ignore))))))
  nil)


;;;; -- autolith.el --

(-> test-autolith-el-compiles () null)
(defun test-autolith-el-compiles ()
  "emacs/autolith.el byte-compiles warning-free without sly or agent-shell installed."
  (let ((emacs (emacs-test-executable)))
    (if (null emacs)
        (test-withheld ':emacs "autolith.el byte compilation")
        (with-test-configuration (base root)
          (declare (ignore base))
          (let ((copy (merge-pathnames "autolith.el" root)))
            (publish-file copy (uiop:read-file-string
                                (merge-pathnames "emacs/autolith.el"
                                                 (asdf:system-source-directory :autolith))))
            (multiple-value-bind (output error-output status)
                (uiop:run-program (list (uiop:native-namestring emacs) "-Q" "--batch"
                                        "--eval" "(setq byte-compile-error-on-warn t)"
                                        "-f" "batch-byte-compile"
                                        (uiop:native-namestring copy))
                                  :output ':string :error-output ':string
                                  :ignore-error-status t)
              (declare (ignore output))
              (test-assert (zerop status)
                           (format nil "autolith.el compiles cleanly: ~A" error-output))
              (test-assert (probe-file (merge-pathnames "autolith.elc" root))
                           "Compilation writes autolith.elc."))))))
  nil)

(-> test-autolith-el-layout () t)
(defun test-autolith-el-layout ()
  "Exercise IDE startup and pane geometry in a clean Emacs process."
  (let ((emacs (emacs-test-executable)))
    (if (null emacs)
        (test-withheld ':emacs "autolith.el IDE layout")
        (let ((source (asdf:system-source-directory :autolith)))
          (multiple-value-bind (output error-output status)
              (uiop:run-program
               (list (uiop:native-namestring emacs) "-Q" "--batch"
                     "--load" (uiop:native-namestring
                               (merge-pathnames "emacs/autolith.el" source))
                     "--load" (uiop:native-namestring
                               (merge-pathnames "tests/autolith-emacs-tests.el" source))
                     "-f" "ert-run-tests-batch-and-exit")
               :output ':string :error-output ':string :ignore-error-status t)
            (test-assert (zerop status)
                         (format nil "IDE layout checks pass: ~A~A" output error-output)))))))
