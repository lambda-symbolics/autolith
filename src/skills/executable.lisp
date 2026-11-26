(in-package #:autolith)

;;;; -- Isolated Executable Skills --

(define-condition executable-skill-error (tool-error)
  ((code :initarg :code :reader executable-skill-error-code
         :documentation "Stable executable skill admission failure."))
  (:documentation "An executable Skill failed product admission or transport."))

(defmethod tool-failure-code ((condition executable-skill-error))
  "Expose executable admission errors at the normal tool boundary."
  (executable-skill-error-code condition))

(defclass executable-skill-tool (tool)
  ((operation :initarg :operation :reader executable-skill-tool-operation
              :documentation "Invoke, self-test, or verify the selected executable."))
  (:documentation "Run an explicitly authorized Skill in a fresh isolated worker."))

(-> executable-skill--fail (keyword string) nil)
(defun executable-skill--fail (code message)
  "Reject admission without loading executable definitions."
  (error 'executable-skill-error :code code :message message :tool-name "skill.invoke"))

(-> executable-skill-discover (configuration string)
    (values cl-skills:skill-executable skill-metadata))
(defun executable-skill-discover (configuration name)
  "Return NAME's executable metadata and Skill metadata without resolving or loading code."
  (let ((metadata (skill-catalog-find (skill-catalog-for-configuration configuration) name)))
    (unless metadata
      (executable-skill--fail ':unknown-skill (format nil "Unknown Skill ~S." name)))
    (let ((executable (cl-skills:skill-executable-discover metadata)))
      (unless executable
        (executable-skill--fail ':not-executable (format nil "Skill ~S has no executable sidecar." name)))
      (values executable metadata))))

(defmethod tool-failure-code ((condition cl-skills:skill-executable-error))
  "Retain library discovery and contract failure codes at the product boundary."
  (cl-skills:skill-executable-error-kind condition))

(-> executable-skill--identity (list) list)
(defun executable-skill--identity (snapshot)
  "Project the reportable version and sidecar identity from SNAPSHOT."
  (loop for key in '(:pathname :digest :skill-version :system :system-version :dependencies)
        append (list key (getf snapshot key))))

(-> executable-skill--admit (tool-context list &key (:asd (option pathname))) list)
(defun executable-skill--admit (context snapshot &key asd)
  "Check declared host-tool requirements and require explicit full-access command authorization for code execution.
Capabilities are declarations for the ordinary command authorizer, not grants.
Only tools declared by exact name enter the host bridge."
  (let* ((manifest  (getf snapshot :manifest))
         (arguments (json-object "host-tools" (coerce (getf manifest :tools) 'vector)))
         (names     (worker-host--allowlist context arguments))
         (identity  (executable-skill--identity snapshot))
         (command   (format nil "skill.execute -- ~S :manifest ~S :asd ~S"
                            identity manifest (and asd (namestring asd)))))
    (unless (eq ':full-access
                (tool-context-authorize-command
                 context command
                 (config :working-directory (tool-context-configuration context))))
      (executable-skill--fail ':not-authorized
                             "Executable Skill code requires explicit full-access command authorization."))
    names))

(-> executable-skill--form (list &key (:operation keyword) (:input t) (:asd (option pathname))) string)
(defun executable-skill--form (snapshot &key operation input asd)
  "Build a worker form that revalidates the exact identity before loading code."
  (let ((identity (executable-skill--identity snapshot)))
    (with-standard-io-syntax
      (let ((*print-readably* t) (*print-pretty* nil))
        (write-to-string
         `(handler-case
              (let ((executable (cl-skills:skill-executable-rediscover ',snapshot)))
                ,@(when asd `((asdf:load-asd (uiop:parse-native-namestring ,(namestring asd)))))
                (flet ((authorize (value &key operation context)
                         (declare (ignore operation context))
                         (cl-skills:skill-executable-identity-matches-p value ',snapshot))
                       (loader (name &key context)
                         (declare (ignore context))
                         (asdf:load-system name)))
                  (list :success-p t :identity ',identity :value
                        ,(if (eq operation ':invoke)
                             `(cl-skills:skill-executable-invoke
                               executable ',input :authorize #'authorize :loader #'loader
                               :context (autolith:worker-tool-context))
                             `(cl-skills:skill-executable-verify
                               executable :entrypoint ',operation :authorize #'authorize :loader #'loader
                               :context (autolith:worker-tool-context))))))
            (cl-skills:skill-executable-error (condition)
              (let ((diagnostic (cl-skills:skill-executable-error-diagnostic condition)))
                (list :success-p   nil :code (cl-skills:skill-executable-error-kind condition)
                      :identity    ',identity
                      :diagnostics (when diagnostic
                                     (list (list :kind     (cl-skills:skill-diagnostic-kind diagnostic)
                                                 :pathname (namestring (cl-skills:skill-diagnostic-pathname diagnostic))
                                                 :message  (cl-skills:skill-diagnostic-message diagnostic))))
                      :message     (princ-to-string condition))))))))))

(defparameter *executable-skill-result-byte-limit* (* 1024 1024)
  "Maximum bytes in one executable result envelope, independently of tool previews.")

(-> executable-skill--result-grammar () source-grammar)
(defun executable-skill--result-grammar ()
  "Return the grammar for portable result data, with limits enforced before construction."
  (make-source-grammar
   :label                                     "Executable Skill result"
   :maximum-depth 128 :maximum-nodes 262144
   :maximum-string-characters                 *executable-skill-result-byte-limit*
   :qualified-common-lisp-symbols-permitted-p t
   :readable-strings-permitted-p              t
   :read-default-float-format                 'single-float
   :allowed-atom-predicate
   (lambda (value)
     (or (null value) (eq value t) (keywordp value) (stringp value)
         (task-output--json-number-p value)))))

(-> executable-skill--check-directory (pathname platform-file-status) null)
(defun executable-skill--check-directory (directory expected)
  "Reject replacement of the publication directory around file acquisition and reading."
  (let ((actual (platform-path-status *platform* directory :follow-links-p nil)))
    (unless (and actual (eq (platform-file-status-kind actual) ':directory)
                 (platform-file-status-same-object-p actual expected))
      (executable-skill--fail ':result-transport "Executable result directory identity changed.")))
  nil)

(-> executable-skill--publication-form (string pathname string) string)
(defun executable-skill--publication-form (source pathname token)
  "Publish SOURCE's portable record without passing through the REPL preview printer."
  (with-standard-io-syntax
    (let ((*read-eval* nil))
      (write-to-string
       `(let ((record ,(read-from-string source)))
          (with-open-file (stream ,(namestring pathname) :direction ':output
                                  :if-exists ':error :if-does-not-exist ':create
                                  :external-format                  ':utf-8)
            (with-standard-io-syntax
              (write (list :token ,token :record record) :stream stream :readably t)))
          t)))))

(-> executable-skill--read-result (pathname string &key (:directory-status platform-file-status)) list)
(defun executable-skill--read-result (pathname token &key directory-status)
  "Read a bounded single envelope with EOF, no reader evaluation, and stable identities."
  (let ((directory (uiop:pathname-directory-pathname pathname)))
    (executable-skill--check-directory directory directory-status)
    (handler-case
        (let ((bytes (ls-compat.files:read-stable-file-octets
                      pathname
                      :maximum-octets      *executable-skill-result-byte-limit*
                      :validation-function
                      (lambda () (executable-skill--check-directory directory directory-status)))))
          (executable-skill--check-directory directory directory-status)
          (handler-case
              (let ((envelope (read-source
                               (babel:octets-to-string bytes :encoding ':utf-8)
                               (executable-skill--result-grammar))))
                (unless (and (proper-list-p envelope) (= 4 (length envelope))
                             (eq (first envelope) ':token)
                             (equal (second envelope) token)
                             (eq (third envelope) ':record)
                             (proper-list-p (fourth envelope)))
                  (executable-skill--fail ':result-transport "Executable result envelope is invalid."))
                (fourth envelope))
            (sexp-config-error (condition)
              (executable-skill--fail ':result-transport (sexp-config-error-message condition)))))
      (ls-compat.files:file-too-large ()
        (executable-skill--fail ':result-transport "Executable result exceeds the transport byte limit."))
      (ls-compat.files:file-changed ()
        (executable-skill--fail ':result-transport "Executable result changed while reading.")))))

(-> executable-skill--record-result (list) tool-result)
(defun executable-skill--record-result (value)
  "Retain complete native results and identity alongside a bounded preview."
  (make-instance 'executable-skill-result
                 :content   (bounded-string
                             (if (getf value :success-p)
                                 (format nil "~S" (getf value :value))
                                 (getf value :message))
                             :overflow-uri-function *tool-result-overflow-function*)
                 :success-p (getf value :success-p)
                 :category  (if (getf value :success-p) ':success ':failure)
                 :error-code (getf value :code) :record value))

(-> executable-skill--run (tool-context string &key (:tools list)) tool-result)
(defun executable-skill--run (context source &key tools)
  "Run SOURCE in a unique disposable heap and clean its private transport on every exit."
  (let* ((identifier (make-identifier))
         (directory  (merge-pathnames (format nil "executable-results/~A/" identifier)
                                      (config :cache-root (tool-context-configuration context))))
         (pathname   (merge-pathnames "result.sexp" directory))
         (worker     (lisp-worker-create (tool-context-configuration context)
                                         :name (format nil "skill-~A" identifier))))
    (unwind-protect
         (progn
           (ensure-directories-exist pathname)
           (platform-make-private *platform* directory)
           (let* ((status   (platform-path-status *platform* directory :follow-links-p nil))
                  (form     (executable-skill--publication-form source pathname identifier))
                  (response (worker-host-eval-request
                             worker ':eval
                             (list :forms (list "(asdf:load-system \"cl-skills/executable\")" form))
                             :context context :tools tools :enabled-p t)))
             ;; Stop the originating heap before observing its publication.
             (lisp-worker-stop worker)
             (if (eq (getf (rest response) :status) ':ok)
                 (handler-case
                     (executable-skill--record-result
                      (executable-skill--read-result pathname identifier :directory-status status))
                   (executable-skill-error (condition) (error condition))
                   (error ()
                     (executable-skill--fail ':result-transport "Could not read executable result publication.")))
                 (worker-response-tool-result response))))
      (unwind-protect (lisp-worker-stop worker)
        (platform-delete-directory-tree *platform* directory :validate t :if-does-not-exist ':ignore)))))

(defclass executable-skill-result (tool-result)
  ((record :initarg :record :reader executable-skill-result-record
           :documentation "Portable validated value and exact executable version identity."))
  (:documentation "Native execution evidence, distinct from prose Skill selection."))

(defmethod tool-result-details ((result executable-skill-result))
  "Retain identity and validated portable result in native tool transport."
  (list :kind ':executable-skill :record (executable-skill-result-record result)))

(defmethod tool-execute ((tool executable-skill-tool) (context tool-context) (arguments hash-table))
  "Admit before code loading, then run with captured policy in a disposable heap."
  (let ((executable (executable-skill-discover (tool-context-configuration context)
                                              (skill-load-tool--name arguments))))
    (let* ((operation (if (eq (executable-skill-tool-operation tool) ':invoke)
                          ':invoke
                          (if (equal (tool-argument arguments "entrypoint") "verify")
                              ':verify ':self-test)))
           (input     (when (eq operation ':invoke)
                        (cl-llm-provider-api:output-json->sexp
                         (tool-argument arguments "input" :required t))))
           (asd       (lisp-tool-asd-pathname context arguments (tool-canonical-name tool)))
           (snapshot  (cl-skills:skill-executable-identity-snapshot executable))
           (names     (executable-skill--admit context snapshot :asd asd))
           (form      (executable-skill--form snapshot :operation operation :input input :asd asd)))
      (lisp-tool-invoke-managed-execution
       context arguments :tool-name (tool-canonical-name tool)
       :summary                     (format nil "Executable Skill ~A (~A)" (getf snapshot :name) operation)
       :operation-function
       (lambda () (executable-skill--run context form :tools names))))))

(-> executable-skill-register-tools (tool-registry) tool-registry)
(defun executable-skill-register-tools (registry)
  "Register invoke and verification without changing prose selection behavior."
  (dolist (entry '(("invoke" :invoke) ("verify" :self-test)))
    (unless (tool-registry-find registry "skill" (first entry))
      (tool-registry-register
       registry
       (make-instance 'executable-skill-tool :namespace "skill" :name (first entry)
                      :operation   (second entry)
                      :description "Run a discovered executable Skill in a fresh worker after explicit code authorization. Only declared exact host tools are available. Verify runs self-test by default; select verify for the verification entrypoint."
                      :parameters
                      (tool-object-schema
                       (json-object "name" (tool-string-property "Exact catalog Skill name.")
                                    "input" (json-object "description" "Invocation input satisfying the manifest contract.")
                                    "entrypoint" (json-object "type" "string" "enum" (vector "self-test" "verify"))
                                    "asd" (tool-string-property "Optional authorized ASDF definition pathname.")
                                     "async" (json-object "type" "boolean")
                                     "completion-policy" (tool-completion-policy-property))
                       (if (eq (second entry) ':invoke) '("name" "input") '("name")))))))
  registry)
