(in-package #:autolith)

;;;; -- LSP Client Behavioral Tests --

(defun lsp-client-tests--configuration (&key (name "test") (command "test-lsp"))
  "Build a small server configuration for client tests."
  (make-instance 'lsp-server-configuration :name name :command command :arguments nil
                 :extensions '(".txt") :language-id "text" :root-markers nil
                 :initialization-options (json-object) :settings (json-object)
                 :timeout-seconds 1 :disabled-p nil))

(defun lsp-client-tests--client (root &key capabilities transport)
  "Build a client without starting a process."
  (let ((client (make-instance 'lsp-client
                               :configuration (lsp-client-tests--configuration)
                               :root (uiop:ensure-directory-pathname root))))
    (setf (lsp-client-capabilities client) (or capabilities (json-object))
          (lsp-client-transport client) transport)
    client))

(defmacro lsp-client-tests--assert (form)
  "Record a client-test assertion with a stable description."
  `(test-assert ,form "LSP client assertion"))

(defmacro lsp-client-tests--with-replacements (replacements &body body)
  "Run BODY while temporarily replacing the listed functions."
  `(test-call-with-function-replacements ,replacements (lambda () ,@body)))

(defun lsp-client-tests--write (configuration name text)
  "Write TEXT beneath CONFIGURATION's temporary workspace."
  (let ((path (merge-pathnames name (test-configuration-root configuration))))
    (ensure-directories-exist path)
    (with-open-file (stream path :direction ':output :if-exists ':supersede :if-does-not-exist ':create)
      (write-string text stream))
    path))

(defun test-lsp-client-position-and-sync-options ()
  "Test UTF-16 positions and advertised synchronization modes."
  (lsp-client-tests--assert (= 3 (json-get (lsp--position "a😀") "character")))
  (with-test-configuration (configuration)
    (let ((client (lsp-client-tests--client (configuration-working-directory configuration))))
      (setf (lsp-client-capabilities client) (json-object "textDocumentSync" 2))
      (multiple-value-bind (kind open save) (lsp-client--sync-options client)
        (lsp-client-tests--assert (and (= kind 2) open (null save))))
      (setf (lsp-client-capabilities client)
            (json-object "textDocumentSync" (json-object "change" 1 "openClose" t
                                                            "save" (json-object "includeText" t))))
      (multiple-value-bind (kind open save) (lsp-client--sync-options client)
        (lsp-client-tests--assert (and (= kind 1) open (json-get save "includeText")))))))

(defun test-lsp-client-handshake-callbacks ()
  "Test initialize negotiation and initialized notifications."
  (with-test-configuration (configuration)
    (let ((requests nil) (notifications nil) (transport (list :live t)))
      (lsp-client-tests--with-replacements
       (list
        (list 'lsp-transport-open (lambda (&rest arguments) (declare (ignore arguments)) transport))
        (list 'lsp-transport-request
              (lambda (ignored method params &key timeout)
                (declare (ignore ignored params timeout))
                (push method requests)
                (json-object "capabilities" (json-object "positionEncoding" "utf-16"))))
        (list 'lsp-transport-notify
              (lambda (ignored method params)
                (declare (ignore ignored params)) (push method notifications)))
        (list 'lsp-transport-close
              (lambda (ignored) (declare (ignore ignored)) (setf (getf transport :live) nil))))
       (let ((client (lsp-client-start (lsp-client-tests--configuration)
                                       (configuration-working-directory configuration))))
         (lsp-client-tests--assert (string= (json-get (lsp-client-capabilities client) "positionEncoding") "utf-16"))
         (lsp-client-tests--assert (equal (reverse requests) '("initialize")))
         (lsp-client-tests--assert (equal (reverse notifications)
                             '("initialized" "workspace/didChangeConfiguration"))))))))

(defun test-lsp-client-full-and-incremental-sync ()
  "Test full-document and ranged incremental synchronization notifications."
  (with-test-configuration (configuration)
    (let* ((path (lsp-client-tests--write configuration "a.txt" "hello"))
           (sent nil)
           (client (lsp-client-tests--client (configuration-working-directory configuration)
                                              :transport (list :live t))))
      (setf (lsp-client-capabilities client)
            (json-object "textDocumentSync" (json-object "change" 2 "openClose" t)))
      (lsp-client-tests--with-replacements
       (list (list 'lsp-transport-notify
                   (lambda (transport method params)
                     (declare (ignore transport)) (push (list method params) sent))))
       (lsp-client-sync client path)
       (with-open-file (stream path :direction ':output :if-exists ':supersede)
         (write-string "hello!" stream))
       (lsp-client-sync client path))
      (lsp-client-tests--assert (= 2 (length sent)))
      (lsp-client-tests--assert (string= (first (first sent)) "textDocument/didChange"))
      (let* ((change (aref (json-get (second (first sent)) "contentChanges") 0))
             (end (json-get (json-get change "range") "end")))
        (lsp-client-tests--assert (and (= (json-get end "line") 0)
                                     (= (json-get end "character") 5)
                                     (string= (json-get change "text") "hello!")))))))

(defun test-lsp-client-diagnostics-and-stale-invalidation ()
  "Test stale diagnostic rejection and invalidation after source changes."
  (with-test-configuration (configuration)
    (let* ((path (lsp-client-tests--write configuration "a.txt" "x"))
           (client (lsp-client-tests--client (configuration-working-directory configuration)
                                              :transport (list :live t))))
      (lsp-client-sync client path)
      (let ((document (gethash (lsp-path-uri path) (lsp-client-documents client))))
        (lsp-client--publish-diagnostics
         client (json-object "uri" (lsp-path-uri path) "version" 1
                             "diagnostics" (vector (json-object "message" "old"))))
        (lsp-client-tests--assert (= 1 (length (lsp-document-diagnostics document))))
        (with-open-file (stream path :direction ':output :if-exists ':supersede)
          (write-string "xx" stream))
        (lsp-client-sync client path)
        (lsp-client-tests--assert (zerop (length (lsp-document-diagnostics document))))
        (lsp-client--publish-diagnostics
         client (json-object "uri" (lsp-path-uri path) "version" 1 "diagnostics" #()))
        (lsp-client-tests--assert (zerop (length (lsp-document-diagnostics document))))))))

(defun test-lsp-client-document-bounds ()
  "Test document byte and open-document limits."
  (with-test-configuration (configuration)
    (let* ((path (lsp-client-tests--write configuration "large.txt" "12345"))
           (client (lsp-client-tests--client (configuration-working-directory configuration)
                                              :transport (list :live t))))
      (let ((*lsp-maximum-document-bytes* 4))
        (lsp-client-tests--assert (handler-case (progn (lsp-client-sync client path) nil)
                       (lsp-error () t))))
      (let ((*lsp-maximum-document-bytes* 100) (*lsp-maximum-open-documents* 0))
        (lsp-client-tests--assert (handler-case (progn (lsp-client-sync client path) nil)
                       (lsp-error () t)))))))

(defun test-lsp-client-diagnostics-push-pull ()
  "Test pending versus empty push reports and pull diagnostics."
  (with-test-configuration (configuration)
    (let* ((path (lsp-client-tests--write configuration "a.txt" "x"))
           (requests 0)
           (client (lsp-client-tests--client (configuration-working-directory configuration)
                                              :transport (list :live t))))
      (lsp-client-sync client path)
      (let ((document (gethash (lsp-path-uri path) (lsp-client-documents client))))
        (let ((pending (lsp-client-diagnostics client document :wait-seconds 0)))
          (lsp-client-tests--assert (string= (json-get pending "state") "pending")))
        (lsp-client--publish-diagnostics
         client (json-object "uri" (lsp-path-uri path) "version" 1 "diagnostics" #()))
        (let ((empty (lsp-client-diagnostics client document :wait-seconds 0)))
          (lsp-client-tests--assert (and (string= (json-get empty "state") "received")
                            (zerop (length (json-get empty "items"))))))
        (setf (lsp-client-capabilities client) (json-object "diagnosticProvider" t))
        (lsp-client-tests--with-replacements
         (list (list 'lsp-transport-request
                     (lambda (&rest arguments)
                       (declare (ignore arguments))
                       (incf requests)
                       (json-object "kind" "full" "items" #()))))
          (let ((empty (lsp-client-diagnostics client document :wait-seconds 0)))
           (lsp-client-tests--assert (and (string= (json-get empty "state") "received")
                             (zerop (length (json-get empty "items")))))
           (lsp-client-tests--assert (= 1 requests))))))))

(defun test-lsp-tool-position-and-query ()
  "Test one-based UTF-16 tool positions and read-only query dispatch."
  (with-test-configuration (configuration)
    (let* ((path (lsp-client-tests--write configuration "a.txt" "a😀
"))
           (document (make-instance 'lsp-document :path path :text "a😀
"))
           (client (lsp-client-tests--client (configuration-working-directory configuration)
                                              :capabilities (json-object "hoverProvider" t)
                                              :transport (list :live t)))
           (arguments (json-object "operation" "hover" "line" 1 "character" 4))
           (observed nil))
      (lsp-client-tests--assert (= 0 (json-get (lsp-tool--position document arguments) "line")))
      (lsp-client-tests--assert (= 3 (json-get (lsp-tool--position document arguments) "character")))
      (lsp-client-tests--with-replacements
       (list (list 'lsp-transport-request
                   (lambda (transport method params &key timeout)
                     (declare (ignore transport timeout))
                     (setf observed (list method params))
                     (json-object "contents" "ok"))))
       (lsp-client-tests--assert (string= (json-get (lsp-tool--query client document arguments) "contents") "ok"))
       (lsp-client-tests--assert (string= (first observed) "textDocument/hover"))))))

(defun test-lsp-client-manager-reuse-restart-and-cleanup ()
  "Test lazy client reuse, dead-client restart, and manager cleanup."
  (with-test-configuration (configuration)
    (let* ((root (configuration-working-directory configuration))
           (server-configuration (lsp-client-tests--configuration))
           (starts 0) (closes 0) (manager (make-instance 'lsp-manager)))
      (lsp-client-tests--with-replacements
       (list
        (list 'lsp-client-start
              (lambda (config directory)
                (declare (ignore config directory))
                (incf starts)
                (lsp-client-tests--client root :transport
                                           (make-instance 'lsp-transport
                                                          :process nil :input nil :output nil
                                                          :error-output nil :request-handler nil
                                                          :notification-handler nil))))
        (list 'lsp-client-close
              (lambda (client &key detach-p)
                (declare (ignore detach-p))
                (incf closes)
                (setf (lsp-client-transport client) nil))))
       (let ((one (lsp-manager-client manager server-configuration root)))
         (lsp-client-tests--assert (eq one (lsp-manager-client manager server-configuration root)))
         (setf (lsp-client-transport one) nil)
         (lsp-manager-client manager server-configuration root)
         (lsp-client-tests--assert (= starts 2))
         (lsp-manager-close manager)
         (lsp-client-tests--assert (= closes 2))
         (lsp-client-tests--assert (zerop (hash-table-count (lsp-manager-clients manager)))))))))

(-> test-lsp-tool-conditional-registration () null)
(defun test-lsp-tool-conditional-registration ()
  "Register the LSP tool surface only when lsp.sexp enables a server."
  (with-test-configuration (configuration)
    (labels ((status (configuration)
               (let ((registry (make-default-tool-registry
                                :configuration configuration)))
                 (unwind-protect (tool-registry-find registry "lsp" "status")
                   (tool-registry-close-runtime-state registry)))))
      (test-assert (null (status configuration))
                   "missing lsp.sexp registers no LSP tools")
      (lsp-configuration-tests--write
       configuration
       "(:version 1 :servers ((:name \"c\" :command \"clangd\" :extensions (\".c\") :language-id \"c\")))")
      (test-assert (status configuration)
                   "an enabled server registers the LSP tools")
      (lsp-configuration-tests--write
       configuration
       "(:version 1 :servers ((:name \"c\" :command \"clangd\" :extensions (\".c\") :language-id \"c\" :disabled-p t)))")
      (test-assert (null (status configuration))
                   "only disabled servers register no LSP tools")
      (lsp-configuration-tests--write configuration "(:version 1 :bogus t)")
      (test-assert (status configuration)
                   "malformed lsp.sexp keeps LSP tools registered so the error surfaces")
      (let ((registry (make-default-tool-registry)))
        (unwind-protect
             (test-assert (null (tool-registry-find registry "lsp" "status"))
                          "registries without a configuration register no LSP tools")
          (tool-registry-close-runtime-state registry)))))
  nil)


(-> test-lsp-session-context () null)
(defun test-lsp-session-context ()
  "Contribute configured server names to provider request context."
  (with-test-configuration (configuration)
    (let ((conversation
            (conversation-create configuration :identifier "lsp-context")))
      (labels ((contribution ()
                 (lsp-context-contribution
                  (make-instance 'request-context
                                 :configuration configuration
                                 :conversation conversation
                                 :tool-namespaces #()))))
        (test-assert (null (contribution))
                     "missing lsp.sexp contributes no session context")
        (lsp-configuration-tests--write
         configuration
         "(:version 1 :servers ((:name \"clangd\" :command \"clangd\" :extensions (\".c\") :language-id \"c\") (:name \"disabled-one\" :command \"x\" :extensions (\".x\") :language-id \"x\" :disabled-p t)))")
        (let* ((contribution (contribution))
               (instruction
                 (and contribution
                      (context-contribution-instruction contribution))))
          (test-assert (typep contribution 'context-contribution)
                       "configured servers contribute session context")
          (test-assert (search "clangd" instruction)
                       "the contribution names enabled servers")
          (test-assert (not (search "disabled-one" instruction))
                       "disabled servers stay unnamed"))
        (lsp-configuration-tests--write configuration "(:version 1 :bogus t)")
        (test-assert (null (contribution))
                     "malformed lsp.sexp contributes no session context")
        (lsp-configuration-tests--write
         configuration
         "(:version 1 :servers ((:name \"c1\" :command \"x\" :extensions (\".c\") :language-id \"c\") (:name \"c2\" :command \"x\" :extensions (\".c\") :language-id \"c\") (:name \"c3\" :command \"x\" :extensions (\".c\") :language-id \"c\") (:name \"c4\" :command \"x\" :extensions (\".c\") :language-id \"c\") (:name \"c5\" :command \"x\" :extensions (\".c\") :language-id \"c\")))")
        (let ((instruction
                (context-contribution-instruction (contribution))))
          (test-assert (and (search "c1" instruction)
                            (search "more" instruction))
                       "long server lists are summarized")))))
  nil)
