(in-package #:autolith)

;;;; -- LSP Integration Tests --

(-> lsp-client-tests--configuration (&key (:name string) (:command string)) lsp-server-configuration)
(defun lsp-client-tests--configuration (&key (name "test") (command "test-lsp"))
  "Build a small server configuration for client tests."
  (make-instance 'lsp-server-configuration :name name :command command :arguments nil
                 :extensions '(".txt") :language-id "text" :root-markers nil
                 :initialization-options (json-object) :settings (json-object)
                 :timeout-seconds 1 :disabled-p nil))

(-> lsp-client-tests--client ((or pathname string) &key (:capabilities t) (:transport t)) lsp-client)
(defun lsp-client-tests--client (root &key capabilities transport)
  "Build a client without starting a process."
  (let ((client (make-instance 'lsp-client
                               :configuration (lsp-client-tests--configuration)
                               :root          (uiop:ensure-directory-pathname root))))
    (setf (lsp-client-capabilities client) (or capabilities (json-object))
          (lsp-client-transport client) transport)
    client))

(defmacro lsp-client-tests--assert (form)
  "Record a client-test assertion with a stable description."
  `(test-assert ,form "LSP client assertion"))

(defmacro lsp-client-tests--with-replacements (replacements &body body)
  "Run BODY while temporarily replacing the listed functions."
  `(test-call-with-function-replacements ,replacements (lambda () ,@body)))

(-> lsp-client-tests--write (configuration string string) pathname)
(defun lsp-client-tests--write (configuration name text)
  "Write TEXT beneath CONFIGURATION's temporary workspace."
  (test-write-text (merge-pathnames name (test-configuration-root configuration))
                   text :external-format ':default))

(-> test-lsp-result-bounds () null)
(defun test-lsp-result-bounds ()
  "Keep bounded LSP documents valid through successful tool result transport."
  (let ((nested 0))
    (dotimes (index 1000) (setf nested (vector nested)))
    (dolist (value (list (json-object "message" (make-string 50000 :initial-element #\"))
                         (make-array 10000 :initial-element (json-object "severity" 1 "message" "error"))
                         (json-object (make-string 30000 :initial-element #\x) "large key")
                         nested))
      (dolist (limit '(64 8000 24000))
        (let* ((text    (lsp-tool--render value limit))
               (result  (tool-success text))
               (decoded (json-decode (tool-result-content result))))
          (test-assert (and (<= (length text) (min limit 8000))
                            (string= text (tool-result-content result))
                            (json-object-p decoded) (json-get decoded "truncated"))
                       "large LSP values retain valid JSON and explicit truncation after transport"))))
    (let* ((value   (json-object "server" "small" "result" (vector 1 "hello")))
           (decoded (json-decode (lsp-tool--render value))))
      (test-assert (and (equal "small" (json-get decoded "server"))
                        (equalp #(1 "hello") (json-get decoded "result"))
                        (not (json-get decoded "truncated")))
                   "complete small results preserve their structured values"))))

(-> test-lsp-tool-position-and-query () null)
(defun test-lsp-tool-position-and-query ()
  "Test one-based UTF-16 tool positions and read-only query dispatch."
  (with-test-configuration (configuration)
    (let* ((path      (lsp-client-tests--write configuration "a.txt" "a😀
"))
           (document  (make-instance 'lsp-document :path path :text "a😀
"))
           (client    (lsp-client-tests--client (config :working-directory configuration)
                                                :capabilities (json-object "hoverProvider" t)
                                                 :transport   (list :live t)))
           (arguments (json-object "operation" "hover" "line" 1 "character" 4)))
      (lsp-client-tests--assert (= 0 (json-get (lsp-tool--position document arguments) "line")))
      (lsp-client-tests--assert (= 3 (json-get (lsp-tool--position document arguments) "character")))
      (lsp-client-tests--with-replacements
       (list (list 'lsp-transport-request
                   (lambda (transport method params &key timeout)
                    (declare (ignore transport method params timeout))
                    (json-object "contents" "ok"))))
       (lsp-client-tests--assert (string= (json-get (lsp-tool--query client document arguments) "contents") "ok")))))
  nil)

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
                                 :configuration   configuration
                                 :conversation    conversation
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

(-> test-lsp-edit-diagnostics-completion () null)
(defun test-lsp-edit-diagnostics-completion ()
  "Finish diagnostic follow-up and preserve saved edits when diagnostics fail."
  (dolist (failure-p '(nil t))
    (with-test-configuration (base-configuration root)
      (let* ((configuration (configuration-copy base-configuration :working-directory root))
             (path          (lsp-client-tests--write configuration "diagnostics.txt" "before"))
             (registry      (lsp-register-tools (make-default-tool-registry)))
             (context       (make-instance 'tool-context :configuration configuration :worker nil
                                           :registry registry :conversation (conversation-create configuration)))
             (reports       (vector (json-object "server" "test" "result"
                                                  (json-object "diagnostics" #()))))
             (observed      ':pending))
        (unwind-protect
             (lsp-client-tests--with-replacements
              (list
               (list 'lsp-tool--call-for-file
                     (lambda (&rest arguments)
                       (declare (ignore arguments))
                       (when failure-p
                         (error 'lsp-error :message "Fixture diagnostics failure."))
                       (sleep 1.1)
                       reports))
               (list 'context-rule-note-diagnostics
                     (lambda (conversation rows &key generation)
                       (declare (ignore conversation generation))
                       (setf observed rows))))
              (let* ((read-result (test-resource-call registry context
                                                    :namespace "resource" :name "read"
                                                    :arguments (list "uri" "workspace:diagnostics.txt")))
                     (revision    (test-resource-field (tool-result-content read-result) "Revision: "))
                     (result      (test-resource-call
                                   registry context :namespace "resource" :name "edit"
                                   :arguments
                                   (list "uri" "workspace:diagnostics.txt" "base-revision" revision
                                         "operations" (vector (json-object "op" "replace-lines" "start-line" 1
                                                                           "end-line" 1 "content" "after"))))))
                (test-assert (and (tool-result-success-p result)
                                  (string= "after" (string-trim '(#\Newline #\Return)
                                                               (uiop:read-file-string path))))
                             "The saved edit is complete after diagnostic success or failure.")
                (test-assert (if failure-p (eq observed ':pending) (eq observed reports))
                             "Completed reports are recorded after the diagnostic request finishes.")))
          (tool-registry-close-runtime-state registry)))))
  nil)
