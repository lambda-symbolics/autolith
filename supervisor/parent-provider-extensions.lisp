;;;; -- Explicit Trusted Parent Provider Extensions --

(in-package #:autolith-supervisor)

(define-condition parent-provider-extension-error (error)
  ((reason :initarg :reason :reader parent-provider-extension-error-reason
           :documentation "Fixed failure category without callback data."))
  (:documentation "A trusted provider extension failed validation or execution.")
  (:report (lambda (condition stream)
             (format stream "Parent provider extension failed: ~A."
                     (parent-provider-extension-error-reason condition)))))

(defvar *parent-provider-extension-registry* nil
  "Registry bound only while trusted parent extension code runs.")

(defvar *parent-provider-extension-source* ':runtime
  "Registration layer bound by the explicit parent extension loader.")

(serapeum/types:-> parent-provider--fail (keyword) nil)
(defun parent-provider--fail (reason)
  "Signal a fixed failure without retaining or printing private callback data."
  (error 'parent-provider-extension-error :reason reason))

(serapeum/types:-> make-parent-provider-extension-registry
    (&key (:cache-read-function (or null function))
          (:cache-write-function (or null function)) (:cache-path (or null pathname)))
    cl-llm-provider-api:provider-registry)
(defun make-parent-provider-extension-registry (&key cache-read-function cache-write-function cache-path)
  "Create the parent's layered registry with the existing discovery/cache protocol.
Use CACHE-PATH only in parent-private storage. Explicit cache callbacks take
precedence. Both callbacks receive the parent configuration, never child data."
  (when (and cache-path
             (not (and (uiop:absolute-pathname-p cache-path) (not (wild-pathname-p cache-path)))))
    (parent-provider--fail ':invalid-cache-path))
  (cl-llm-provider-api:provider-registry-create
   :sources '(:builtin :site :user :runtime) :default-context-window 272000
   :cache-read-function
   (or cache-read-function
       (when cache-path
         (lambda (configuration)
           (declare (ignore configuration))
           (when (probe-file cache-path)
             (multiple-value-bind (form sole-form-p) (sexp-store:snapshot-read cache-path)
               (unless sole-form-p (parent-provider--fail ':invalid-model-cache))
               form)))))
   :cache-write-function
   (or cache-write-function
       (when cache-path
         (lambda (configuration form)
           (declare (ignore configuration))
           (ensure-directories-exist cache-path)
           (sexp-store:snapshot-write cache-path form :mode #o600))))))

(serapeum/types:-> register-parent-provider (string &rest t) string)
(defun register-parent-provider (name &rest arguments)
  "Register executable provider callbacks in the currently bound trusted parent.
Accept the REGISTER-PROVIDER factory, authenticator, model-discovery, and endpoint
resolver keywords. Source defaults to the loader's layer. This API never runs
from the agent's ordinary init file. Parent extensions have full parent authority."
  (unless *parent-provider-extension-registry*
    (parent-provider--fail ':registration-outside-parent-extension))
  (unless (member :source arguments)
    (setf arguments (append arguments (list :source *parent-provider-extension-source*))))
  (apply #'cl-llm-provider-api:provider-registry-register
         *parent-provider-extension-registry* name arguments))

(serapeum/types:-> unregister-parent-provider (string &key (:source keyword)) boolean)
(defun unregister-parent-provider (name &key (source *parent-provider-extension-source*))
  "Remove NAME from one trusted registry source, exposing any lower layer."
  (unless *parent-provider-extension-registry*
    (parent-provider--fail ':registration-outside-parent-extension))
  (not (null (cl-llm-provider-api:provider-registry-unregister
              *parent-provider-extension-registry* name :source source))))

(serapeum/types:-> parent-provider--resolved-path (t) pathname)
(defun parent-provider--resolved-path (path)
  "Resolve an absolute existing path, including links, without exposing failures."
  (unless (and (typep path '(or pathname string))
               (uiop:absolute-pathname-p path) (not (wild-pathname-p path)))
    (parent-provider--fail ':invalid-extension-path))
  (handler-case
      (let ((resolved (ls-compat.posix:resolve-pathname (pathname path))))
        (unless (probe-file resolved)
          (parent-provider--fail ':missing-extension-path))
        resolved)
    (parent-provider-extension-error (condition)
      (error condition))
    (error ()
      (parent-provider--fail ':unresolvable-extension-path))))

(serapeum/types:-> parent-provider--within-p (pathname pathname) boolean)
(defun parent-provider--within-p (path root)
  "Compare resolved components, including host-specific case rules."
  (let* ((relative (enough-namestring path (uiop:ensure-directory-pathname root)))
         (pathname (pathname relative)))
    (and (not (uiop:absolute-pathname-p pathname))
         (not (member :up (pathname-directory pathname)))
         (not (member :back (pathname-directory pathname))) t)))

(serapeum/types:-> parent-provider-validate-extension-path (t list) pathname)
(defun parent-provider-validate-extension-path (path writable-roots)
  "Require an existing trusted file outside all child writable roots and aliases.
Call again before each load. Include workspace, temporary and site roots, private
aliases, and granted writable scopes. Retain this file as a denied policy path."
  (let ((resolved (parent-provider--resolved-path path)))
    (when (uiop:directory-pathname-p resolved)
      (parent-provider--fail ':extension-is-directory))
    (dolist (root writable-roots)
      (when (parent-provider--within-p resolved (parent-provider--resolved-path root))
        (parent-provider--fail ':extension-in-child-writable-scope)))
    resolved))

(serapeum/types:-> parent-provider-load-extensions
    (cl-llm-provider-api:provider-registry list &key (:writable-roots list)) list)
(defun parent-provider-load-extensions (registry specifications &key writable-roots)
  "Load only explicit (:SOURCE layer :PATH absolute-file) specifications.
Validate the whole set before executing any file. Replace site and user layers
atomically on load failure. Return resolved files for permanent policy exclusion.
Do not infer files from workspace, ordinary init, or environment search paths."
  (unless (and writable-roots (listp specifications) (list-length specifications))
    (parent-provider--fail ':invalid-extension-specifications))
  (let ((files
          (loop for spec in specifications
                collect
                (progn
                  (unless (and (listp spec) (= (or (list-length spec) 0) 4)
                               (member (getf spec :source) '(:site :user))
                               (member :path spec) (member :source spec))
                    (parent-provider--fail ':invalid-extension-specification))
                  (cons (getf spec :source)
                        (parent-provider-validate-extension-path (getf spec :path) writable-roots)))))
        (snapshot (cl-llm-provider-api:provider-registry-snapshot registry)))
    (handler-case
        (let ((*parent-provider-extension-registry* registry)
              (*standard-output*                    (make-broadcast-stream))
              (*error-output*                       (make-broadcast-stream))
              (*trace-output*                       (make-broadcast-stream)))
          (dolist (source '(:site :user))
            (cl-llm-provider-api:provider-registry-remove-source registry source))
          (dolist (file files)
            (let ((*parent-provider-extension-source* (first file))
                  (*package*                          (find-package '#:autolith-supervisor)))
              (load (parent-provider-validate-extension-path (rest file) writable-roots)
                    :verbose nil :print nil)))
          (mapcar #'rest files))
      (error ()
        (cl-llm-provider-api:provider-registry-restore registry snapshot)
        (parent-provider--fail ':extension-load-failed)))))

(serapeum/types:-> parent-provider--find
    (cl-llm-provider-api:provider-registry string) cl-llm-provider-api:provider-registration)
(defun parent-provider--find (registry name)
  "Find a parent registration without including private data in failure output."
  (or (cl-llm-provider-api:provider-registry-find registry name)
      (parent-provider--fail ':unknown-provider)))

(serapeum/types:-> parent-provider-create
    (cl-llm-provider-api:provider-registry string t
     &key (:reasoning-summaries-p boolean)) cl-llm-provider-api:model-provider)
(defun parent-provider-create (registry name configuration &key reasoning-summaries-p)
  "Execute NAME's factory only in the parent and return its private provider."
  (let ((*parent-provider-extension-registry* registry)
        (*parent-provider-extension-source*   ':runtime)
        (registration                         (parent-provider--find registry name))
        (*standard-output*                    (make-broadcast-stream))
        (*error-output*                       (make-broadcast-stream))
        (*trace-output*                       (make-broadcast-stream)))
    (handler-case
        (let ((provider
                (funcall (cl-llm-provider-api:provider-registration-factory registration)
                         configuration :reasoning-summaries-p reasoning-summaries-p)))
          (unless (typep provider 'cl-llm-provider-api:model-provider)
            (parent-provider--fail ':invalid-factory-result))
          (setf (cl-llm-provider-api:model-provider-registration provider) registration)
          provider)
      (error ()
        (parent-provider--fail ':factory-failed)))))

(serapeum/types:-> parent-provider-authenticate
    (cl-llm-provider-api:provider-registry string t
     &key (:stream stream) (:open-browser-p boolean) (:method (or null keyword))) boolean)
(defun parent-provider-authenticate (registry name configuration
                                     &key (stream *standard-output*) (open-browser-p t) method)
  "Run custom authentication only on the trusted terminal.
Explicit browser/device METHOD selection belongs only to the built-in ChatGPT adapter."
  (when method
    (parent-provider--fail ':unsupported-authentication-method))
  (let* ((*parent-provider-extension-registry* registry)
         (*parent-provider-extension-source*   ':runtime)
         (registration                         (parent-provider--find registry name))
         (authenticator                        (cl-llm-provider-api:provider-registration-authenticator registration)))
    (unless authenticator
      (parent-provider--fail ':authentication-unavailable))
    (let ((provider (parent-provider-create registry name configuration)))
      (handler-case
          (progn
            (funcall authenticator provider :stream stream :open-browser-p open-browser-p)
            t)
        (error ()
          (parent-provider--fail ':authentication-failed))))))

(serapeum/types:-> parent-provider-load-model-cache
    (cl-llm-provider-api:provider-registry t) null)
(defun parent-provider-load-model-cache (registry configuration)
  "Adopt successful cached metadata using the shared endpoint/source cache identity."
  (let ((*parent-provider-extension-registry* registry)
        (*parent-provider-extension-source*   ':runtime)
        (*standard-output*                    (make-broadcast-stream))
        (*error-output*                       (make-broadcast-stream))
        (*trace-output*                       (make-broadcast-stream)))
    (handler-case
        (cl-llm-provider-api:provider-registry-load-model-cache registry configuration)
      (error ()
        (parent-provider--fail ':model-cache-load-failed)))))

(serapeum/types:-> parent-provider-refresh-models
    (cl-llm-provider-api:provider-registry t &key (:provider-name (or null string))) list)
(defun parent-provider-refresh-models (registry configuration &key provider-name)
  "Refresh parent callbacks; retain successful models on failure.
Return only fixed failure records. Never return callback conditions to the child."
  (let ((*parent-provider-extension-registry* registry)
        (*parent-provider-extension-source*   ':runtime)
        (*standard-output*                    (make-broadcast-stream))
        (*error-output*                       (make-broadcast-stream))
        (*trace-output*                       (make-broadcast-stream)))
    (handler-case
        (mapcar (lambda (condition)
                  (declare (ignore condition))
                  '(:reason :model-discovery-failed))
                (cl-llm-provider-api:provider-registry-refresh-models
                 registry configuration :provider-name provider-name))
      (error ()
        (parent-provider--fail ':model-discovery-failed)))))

(serapeum/types:-> parent-provider--public-string (t (integer 1)) string)
(defun parent-provider--public-string (value limit)
  "Copy bounded public metadata without terminal controls."
  (unless (and (stringp value) (<= (length value) limit)
               (every (lambda (character)
                        (and (>= (char-code character) 32)
                             (not (<= 127 (char-code character) 159)))) value))
    (parent-provider--fail ':invalid-public-metadata))
  (copy-seq value))

(serapeum/types:-> parent-provider-projection (cl-llm-provider-api:provider-registry) list)
(defun parent-provider-projection (registry)
  "Return bounded inert names and model metadata for child completion and selection.
Exclude endpoints, callback objects, configurations, credentials, and private errors."
  (let ((registrations (cl-llm-provider-api:provider-registry-registrations registry)))
    (when (> (length registrations) 256)
      (parent-provider--fail ':too-many-providers))
    (loop for registration in registrations
          for models = (cl-llm-provider-api:provider-registration-models registration)
          do (when (> (length models) 4096) (parent-provider--fail ':too-many-models))
          collect
          (list :name (parent-provider--public-string
                       (cl-llm-provider-api:provider-registration-name registration) 256)
                :models
                (loop for model in models
                      collect
                      (list :name           (parent-provider--public-string
                                             (cl-llm-provider-api:provider-model-name model) 512)
                            :context-window (cl-llm-provider-api:provider-model-context-window model)
                            :reasoning-efforts
                            (let ((efforts (cl-llm-provider-api:provider-model-reasoning-efforts model)))
                              (when (> (length efforts) 32)
                                (parent-provider--fail ':too-many-reasoning-efforts))
                              (mapcar (lambda (effort) (parent-provider--public-string effort 64))
                                      efforts))))))))

(serapeum/types:-> parent-provider-stream-turn
    (cl-llm-provider-api:provider-registry string
     &key (:configuration t) (:conversation t) (:tool-namespaces t)
          (:event-callback function) (:goal-context t)
          (:compaction-p boolean) (:reasoning-summaries-p boolean))
    cl-llm-provider-api:provider-result)
(defun parent-provider-stream-turn (registry name &key configuration conversation
                                      tool-namespaces event-callback goal-context compaction-p
                                      reasoning-summaries-p)
  "Run custom provider transport in the parent on a parent-owned conversation.
EVENT-CALLBACK must be the model service's validated semantic event adapter. Its
output still passes the normal model-service protocol and private-data checks."
  (let ((*parent-provider-extension-registry* registry)
        (*parent-provider-extension-source*   ':runtime)
        (provider                             (parent-provider-create registry name configuration
                                                                      :reasoning-summaries-p reasoning-summaries-p))
        (*standard-output*                    (make-broadcast-stream))
        (*error-output*                       (make-broadcast-stream))
        (*trace-output*                       (make-broadcast-stream)))
    (handler-case
        (let ((result
                (cl-llm-provider-api:provider-stream-turn
                 provider conversation :tool-namespaces tool-namespaces
                 :event-callback event-callback :goal-context goal-context :compaction-p compaction-p)))
          (unless (typep result 'cl-llm-provider-api:provider-result)
            (parent-provider--fail ':invalid-transport-result))
          result)
      (error ()
        (parent-provider--fail ':transport-failed)))))
