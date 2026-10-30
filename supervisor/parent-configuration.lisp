;;;; -- Trusted Parent Application Configuration --

(in-package #:autolith-supervisor)

(define-condition parent-configuration-error (error)
  ((reason :initarg :reason :reader parent-configuration-error-reason
           :documentation "A fixed configuration failure category."))
  (:documentation "The trusted parent configuration is invalid.")
  (:report (lambda (condition stream)
             (format stream "Invalid parent configuration: ~A."
                     (parent-configuration-error-reason condition)))))

(defclass parent-configuration ()
  ((values :initarg :values :reader parent-configuration-values
           :documentation "Validated trusted owner data, never agent input.")
   (source-path :initform nil :accessor parent-configuration-source-path
                :documentation "Private owner configuration file, when read from disk.")
   (provider-registry :initform nil :accessor parent-configuration-provider-registry
                      :documentation "Parent-only executable registry, never part of serialized settings.")
   (provider-paths :initform nil :accessor parent-configuration-provider-paths
                   :documentation "Resolved trusted extension files denied to children."))
  (:documentation "Validated owner data with private runtime provider bindings."))

(serapeum/types:-> parent-configuration--reject (keyword) nil)
(defun parent-configuration--reject (reason)
  "Signal a fixed error without printing private configuration."
  (error 'parent-configuration-error :reason reason))

(serapeum/types:-> parent-configuration--data-p (t) boolean)
(defun parent-configuration--data-p (data)
  "Accept a bounded acyclic data tree without executable or opaque objects."
  (let ((remaining 65536) (seen (make-hash-table :test #'eq)))
    (labels ((visit (value depth)
               (and (plusp (decf remaining)) (<= depth 32)
                    (typecase value
                      (cons
                       (and (not (gethash value seen))
                            (progn (setf (gethash value seen) t)
                                   (and (visit (first value) (1+ depth))
                                        (visit (rest value) depth)))))
                      ((or string pathname real null)
                       t)
                      (symbol
                       (or (eq value t) (keywordp value)))
                      (t
                       nil)))))
      (not (null (visit data 0))))))

(serapeum/types:-> parent-configuration--plist (t list) boolean)
(defun parent-configuration--plist (data allowed)
  "Validate a proper keyword plist with unique supported keys."
  (and (listp data)
       (let ((length (list-length data)) (seen nil))
         (and length (evenp length)
             (loop for (key) on data by (lambda (tail) (rest (rest tail)))
                    always (and (member key allowed) (not (member key seen))
                                (progn (push key seen) t)))))))

(serapeum/types:-> parent-configuration-get (parent-configuration keyword) t)
(defun parent-configuration-get (configuration key)
  "Read one trusted setting. Do not send this object to the agent."
  (getf (parent-configuration-values configuration) key))

(serapeum/types:-> make-parent-configuration (list) parent-configuration)
(defun make-parent-configuration (data)
  "Validate trusted owner data. All paths must be explicit and absolute.
PROFILES contains factory keyword plists, without credential-manager objects.
ACCOUNT-CONFIGURATION contains parent-only account settings."
  (unless (parent-configuration--data-p data)
    (parent-configuration--reject ':invalid-data))
  (unless (parent-configuration--plist
           data '(:version :runtime :agent-core :worker-core :source-root :workspace
                  :private-root :temporary-root :runtime-roots :profiles :profile-id
                  :account-configuration :access-p :network-p :reasoning-effort :knowledge-mode :allow-global-p
                  :settings :preferences-path :legacy-conversations-root :data-root :state-root
                  :config-root :provider-extensions))
    (parent-configuration--reject ':unknown-or-duplicate-setting))
  (unless (eql (getf data :version) 1)
    (parent-configuration--reject ':unsupported-version))
  (dolist (key '(:runtime :agent-core :worker-core :source-root :workspace :private-root :temporary-root))
    (let ((path (getf data key)))
      (unless (and (stringp path) (plusp (length path))
                   (uiop:absolute-pathname-p path) (not (wild-pathname-p path)))
        (parent-configuration--reject ':invalid-path))))
  (unless (and (member (getf data :access-p) '(nil t))
               (member (getf data :network-p) '(nil t))
               (listp (getf data :runtime-roots))
               (every (lambda (path) (and (stringp path) (uiop:absolute-pathname-p path)))
                      (getf data :runtime-roots))
               (getf data :runtime-roots))
    (parent-configuration--reject ':invalid-access-setting))
  (let ((profiles (getf data :profiles)) (identifiers nil))
    (unless (and (listp profiles) (list-length profiles) profiles)
      (parent-configuration--reject ':missing-profiles))
    (dolist (profile profiles)
      (unless (and (parent-configuration--plist
                    profile '(:id :family :model :protocol :endpoint :provider-name :headers
                              :options :capabilities :maximum-output-tokens :reasoning-efforts
                              :reasoning-summaries))
                   (family-account-non-empty-string-p (getf profile :id))
                   (family-account-non-empty-string-p (getf profile :model))
                   (member (getf profile :family)
                           '(:codex :grok :fireworks :openai-api :anthropic :mistral :openrouter
                              :opencode :openai-compatible :nous :gemini :copilot :custom))
                   (or (not (eq (getf profile :family) ':custom))
                       (family-account-non-empty-string-p (getf profile :provider-name)))
                   (not (member (getf profile :id) identifiers :test #'equal)))
        (parent-configuration--reject ':invalid-profile))
      (push (getf profile :id) identifiers))
    (unless (member (getf data :profile-id) identifiers :test #'equal)
      (parent-configuration--reject ':unknown-selected-profile)))
  ;; Account validation has its own strict key and value policy.
  (let ((accounts (make-provider-account-configuration (getf data :account-configuration))))
    (dolist (key '(:state-root :api-keys-path :grok-bootstrap-auth-path))
      (let ((path (provider-account-config key accounts)))
        (when (and path (not (uiop:absolute-pathname-p path)))
          (parent-configuration--reject ':invalid-account-path)))))
  (unless (or (null (getf data :reasoning-effort)) (stringp (getf data :reasoning-effort)))
    (parent-configuration--reject ':invalid-reasoning-effort))
  (unless (and (member (getf data :allow-global-p) '(nil t))
               (member (getf data :knowledge-mode "active") '("active" "passive" "off")
                       :test #'equal))
    (parent-configuration--reject ':invalid-knowledge-setting))
  (dolist (key '(:preferences-path :legacy-conversations-root :data-root :state-root :config-root))
    (let ((path (getf data key)))
      (when (and path (not (and (stringp path) (uiop:absolute-pathname-p path)
                                (not (wild-pathname-p path)))))
        (parent-configuration--reject ':invalid-owner-path))))
  (let ((settings (getf data :settings)))
    (unless (and (listp settings) (list-length settings) (evenp (length settings))
                 (loop for (key) on settings by (lambda (tail) (rest (rest tail)))
                       always (keywordp key)))
      (parent-configuration--reject ':invalid-owner-settings)))
  (dolist (specification (getf data :provider-extensions))
    (unless (and (parent-configuration--plist specification '(:source :path))
                 (= (length specification) 4)
                 (member (getf specification :source) '(:site :user))
                 (stringp (getf specification :path))
                 (uiop:absolute-pathname-p (getf specification :path))
                 (not (wild-pathname-p (getf specification :path))))
      (parent-configuration--reject ':invalid-provider-extension)))
  (let ((copy (copy-tree data)))
    (setf (getf copy :knowledge-mode) (getf data :knowledge-mode "active"))
    (dolist (key '(:source-root :workspace :private-root :temporary-root))
      (setf (getf copy key) (namestring (uiop:ensure-directory-pathname (getf copy key)))))
    (make-instance 'parent-configuration :values copy)))

(serapeum/types:-> read-parent-configuration (pathname) parent-configuration)
(defun read-parent-configuration (pathname)
  "Read exactly one bounded data form with reader evaluation disabled."
  (when (> (or (with-open-file (stream pathname :element-type '(unsigned-byte 8))
                (file-length stream)) 0) 1048576)
    (parent-configuration--reject ':configuration-too-large))
  (with-open-file (stream pathname)
    (let* ((*read-eval* nil)
           (*package*   (find-package '#:cl-user))
           (eof         (list nil))
           (data        (read stream nil eof)))
      (unless (and (not (eq data eof)) (eq (read stream nil eof) eof))
        (parent-configuration--reject ':expected-one-record))
      (let ((configuration (make-parent-configuration data)))
        (setf (parent-configuration-source-path configuration)
              (ls-compat.posix:resolve-pathname pathname))
        configuration))))

(serapeum/types:-> parent-configuration-child-environment (parent-configuration) list)
(defun parent-configuration-child-environment (configuration)
  "Build the complete child environment. Never inherit the parent environment."
  (let ((temporary (parent-configuration-get configuration ':temporary-root)))
    (list (concatenate 'string "HOME=" temporary)
          (concatenate 'string "TMPDIR=" temporary)
          (concatenate 'string "XDG_DATA_HOME=" temporary)
          (concatenate 'string "XDG_STATE_HOME=" temporary)
          (concatenate 'string "XDG_CACHE_HOME=" temporary)
          (concatenate 'string "XDG_CONFIG_HOME=" temporary)
          (concatenate 'string "AUTOLITH_SOURCE_ROOT="
                       (parent-configuration-get configuration ':source-root))
          (concatenate 'string "AUTOLITH_SBCL="
                       (parent-configuration-get configuration ':runtime))
          "LANG=en_US.UTF-8")))

(defparameter *parent-configuration-public-setting-keys*
  (copy-list *protocol-application-setting-keys*)
  "The shared public scalar setting contract.")

(serapeum/types:-> parent-configuration-public-settings (parent-configuration) hash-table)
(defun parent-configuration-public-settings (configuration)
  "Copy only approved scalar settings into the child projection."
  (let ((map      (make-hash-table :test #'equal))
        (settings (parent-configuration-get configuration ':settings)))
    (dolist (key *parent-configuration-public-setting-keys*)
      (when (member key settings)
        (let ((value (getf settings key)))
          (unless (typep value '(or null string real (eql t) keyword))
            (parent-configuration--reject ':invalid-public-setting))
          (setf (gethash (string-downcase key) map)
                (cond
                  ((and value (keywordp value))
                   (string-downcase value))
                  ((and (realp value) (not (integerp value)))
                   (coerce value 'double-float))
                  (t
                   value))))))
    map))

(serapeum/types:-> parent-configuration-agent-map
                   (parent-configuration &key (:command-policy (member :deny :inherited-scope))
                    (:profile (or null model-profile))) t)
(defun parent-configuration-agent-map (configuration &key (command-policy ':deny) profile)
  "Project public settings. Only the externally confined parent may assert inherited command scope."
  (let* ((specification (find (parent-configuration-get configuration ':profile-id)
                              (parent-configuration-get configuration ':profiles)
                              :key (lambda (value) (getf value :id)) :test #'equal))
         (map           (make-hash-table :test #'equal)))
    (setf (gethash "profile_id" map) (getf specification :id)
          (gethash "model" map) (getf specification :model)
          (gethash "workspace" map) (parent-configuration-get configuration ':workspace)
          (gethash "worker_core" map) (parent-configuration-get configuration ':worker-core)
          (gethash "knowledge_mode" map) (parent-configuration-get configuration ':knowledge-mode)
          (gethash "reasoning_effort" map)
          (or (parent-configuration-get configuration ':reasoning-effort) "none"))
    (setf (gethash "settings" map) (parent-configuration-public-settings configuration))
    (when profile
      (setf (gethash "reasoning_capabilities" map) (model-profile-reasoning-capabilities profile)))
    (when (eq command-policy ':inherited-scope)
      (setf (gethash "command_policy" map) "inherited-native-scope"))
    (protocol-data->map map)))
