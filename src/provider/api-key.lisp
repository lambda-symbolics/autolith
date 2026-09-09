(in-package #:autolith)

;;;; -- Persistent API-Key Credential Sources --

(defparameter *api-key-store-version* 1
  "The portable version of Autolith's OpenAI-compatible API-key store.")

(defclass api-key-credential-source (autolith-credential-source)
  ((provider-name
    :initarg :provider-name
    :reader api-key-credential-source-provider-name
    :type non-empty-string
    :documentation "The provider name associated with this key source."))
  (:documentation
   "A private persistent credential source for one API-key provider."))

(-> api-key--canonical-provider-name (string) string)
(defun api-key--canonical-provider-name (provider-name)
  "Return the case-insensitive store key for PROVIDER-NAME."
  (string-downcase provider-name))

(-> api-key--call-with-store-lock (pathname function) t)
(defun api-key--call-with-store-lock (pathname function)
  "Call FUNCTION while holding the process and filesystem API-key locks."
  (let ((lock-pathname
          (merge-pathnames
           "api-keys.lock"
           (uiop:pathname-directory-pathname pathname))))
    (handler-case
        (call-with-file-lock lock-pathname function)
      (authentication-error (condition)
        (error condition))
      (error (cause)
        (error 'authentication-error
               :message
               (format nil "Could not lock the API-key store at ~A: ~A"
                       lock-pathname cause))))))

(-> api-key--invalid-store (pathname) null)
(defun api-key--invalid-store (pathname)
  "Signal that PATHNAME is not a valid private API-key store."
  (error 'authentication-error
         :message (format nil "Invalid Autolith API-key store at ~A." pathname)))

(-> api-key--entry-provider-name (list) (option string))
(defun api-key--entry-provider-name (entry)
  "Return the provider name from one validated or unvalidated ENTRY."
  (and (listp entry)
       (getf (rest entry) :provider-name)))

(-> api-key--entry-key (list) (option string))
(defun api-key--entry-key (entry)
  "Return the secret key from one validated or unvalidated ENTRY."
  (and (listp entry)
       (getf (rest entry) :api-key)))

(-> api-key--validate-entries (list pathname) list)
(defun api-key--validate-entries (entries pathname)
  "Validate API-key ENTRIES read from PATHNAME and return them unchanged."
  (unless (listp entries)
    (api-key--invalid-store pathname))
  (let ((seen nil))
    (dolist (entry entries)
      (let ((provider-name (api-key--entry-provider-name entry))
            (api-key (api-key--entry-key entry)))
        (unless (and (listp entry)
                     (eq (first entry) :provider)
                     (non-empty-string-p provider-name)
                     (non-empty-string-p api-key))
          (api-key--invalid-store pathname))
        (let ((canonical-name (api-key--canonical-provider-name provider-name)))
          (when (member canonical-name seen :test #'string=)
            (api-key--invalid-store pathname))
          (push canonical-name seen))))
    entries))

(-> api-key--read-entries (pathname) list)
(defun api-key--read-entries (pathname)
  "Read and validate the provider entries in PATHNAME, or return NIL when absent."
  (if (not (probe-file pathname))
      nil
      (handler-case
          (let* ((record (read-portable-form pathname))
                 (version (and (listp record) (getf (rest record) :version)))
                 (entries (and (listp record)
                               (eq (first record) :api-keys)
                               (getf (rest record) :providers))))
            (unless (and (listp record)
                         (eq (first record) :api-keys)
                         (= version *api-key-store-version*))
              (api-key--invalid-store pathname))
            (api-key--validate-entries entries pathname))
        (authentication-error (condition)
          (error condition))
        (error ()
          (api-key--invalid-store pathname)))))

(-> api-key--entry-for (string list) (option list))
(defun api-key--entry-for (provider-name entries)
  "Find the API-key entry for PROVIDER-NAME in ENTRIES."
  (find (api-key--canonical-provider-name provider-name)
        entries
        :key (lambda (entry)
               (api-key--canonical-provider-name
                (api-key--entry-provider-name entry)))
        :test #'string=))

(defmethod credential-source-label ((source api-key-credential-source))
  "Name the API-key provider in credential failures."
  (api-key-credential-source-provider-name source))

(defmethod credential-source-load ((source api-key-credential-source))
  "Load SOURCE's API key into request scope, without retaining it in the image."
  (let ((pathname (credential-source-pathname source)))
    (api-key--call-with-store-lock
     pathname
     (lambda ()
       (let* ((entry (api-key--entry-for
                      (api-key-credential-source-provider-name source)
                      (api-key--read-entries pathname)))
              (access-token (and entry (api-key--entry-key entry))))
         (when (non-empty-string-p access-token)
           (make-instance
            'oauth-credentials
            :access-token access-token
            :refresh-token nil
            :id-token nil
            :account-id
            (format nil "api-key/~A"
                    (api-key--canonical-provider-name
                     (api-key-credential-source-provider-name source)))
            :expires-at nil
            :source-path pathname)))))))

(defmethod credential-source-save ((source api-key-credential-source)
                                   (credentials oauth-credentials))
  "Atomically save CREDENTIALS' API key in SOURCE's private store."
  (call-with-secret-use
   (lambda ()
     (let ((api-key (oauth-credentials-access-token credentials)))
       (unless (non-empty-string-p api-key)
         (error 'authentication-error
                :message (format nil "The ~A API key is empty."
                                 (credential-source-label source))))
       (let* ((pathname (credential-source-pathname source))
              (provider-name
                (api-key--canonical-provider-name
                 (api-key-credential-source-provider-name source))))
         (api-key--call-with-store-lock
          pathname
          (lambda ()
            (let* ((entries (api-key--read-entries pathname))
                   (updated-entry
                     (list :provider
                           :provider-name provider-name
                           :api-key api-key)))
              (ensure-directories-exist pathname)
              (snapshot-write
               pathname
               (list :api-keys
                     :version *api-key-store-version*
                     :providers
                     (append
                      (remove-if
                       (lambda (entry)
                         (string= provider-name
                                  (api-key--canonical-provider-name
                                   (api-key--entry-provider-name entry))))
                       entries)
                      (list updated-entry))
               :mode #o600))))))
       credentials))))


;;;; -- Interactive API-Key Input --
(defparameter *api-key-input-echo-disabled-p* nil
  "Whether the dynamically bound API-key input transport already suppresses echo.")

(defparameter *api-key-input-file-descriptor* nil
  "The known descriptor for dynamically bound standard API-key input.")

(defparameter *api-key-output-styled-p* nil
  "Whether API-key prompts use trusted semantic terminal styling.")

(-> api-key--strip-bracketed-paste (string) string)
(defun api-key--strip-bracketed-paste (text)
  "Remove one terminal bracketed-paste wrapper from TEXT."
  (let* ((escape (code-char 27))
         (start  (format nil "~C[200~~" escape))
         (end    (format nil "~C[201~~" escape)))
    (if (and (uiop:string-prefix-p start text)
             (uiop:string-suffix-p text end)
             (>= (length text) (+ (length start) (length end))))
        (subseq text (length start) (- (length text) (length end)))
        text)))

(-> api-key--input-file-descriptor (stream (option integer)) (option integer))
(defun api-key--input-file-descriptor (input configured)
  "Return INPUT's configured or direct file descriptor."
  (or configured
      (ignore-errors (sb-sys:fd-stream-fd input))))

(-> api-key--interactive-file-descriptor-p (integer) boolean)
(defun api-key--interactive-file-descriptor-p (file-descriptor)
  "Return true when FILE-DESCRIPTOR names an interactive terminal."
  (platform-interactive-descriptor-p *platform* file-descriptor))

(-> api-key--hidden-input-mode (stream (option integer)) (option cons))
(defun api-key--hidden-input-mode (input configured-descriptor)
  "Hide terminal echo for INPUT's known descriptor and return the mode to restore."
  (let ((descriptor
          (api-key--input-file-descriptor input configured-descriptor)))
    (when (and (null descriptor)
               (not *api-key-input-echo-disabled-p*))
      (error 'authentication-error
             :message
             "Could not identify the input descriptor for API-key entry; no key was read."))
    (when (and descriptor
               (api-key--interactive-file-descriptor-p descriptor))
      (handler-case
          (cons descriptor
                (platform-disable-input-echo *platform* descriptor))
        (error ()
          (error 'authentication-error
                 :message
                 "Could not disable terminal echo for API-key entry; no key was read."))))))

(-> api-key--restore-input-mode (cons) null)
(defun api-key--restore-input-mode (saved-mode)
  "Restore one terminal mode returned by API-KEY--HIDDEN-INPUT-MODE."
  (platform-restore-input-echo *platform* (first saved-mode) (rest saved-mode))
  nil)

(-> api-key--write-prompt-span (stream terminal-style string) null)
(defun api-key--write-prompt-span (stream style text)
  "Write sanitized API-key prompt TEXT to STREAM in STYLE when enabled."
  (let ((sequence (and *api-key-output-styled-p*
                       (terminal-style-sequence style))))
    (when sequence
      (write-string sequence stream))
    (write-string (sanitize-text text :single-line-p t) stream)
    (when sequence
      (write-string (terminal-style-reset-sequence) stream)))
  nil)

(-> api-key-read-hidden
    (string &key
            (:input stream)
            (:input-file-descriptor (option integer))
            (:stream stream)
            (:note (option string)))
    (option string))
(defun api-key-read-hidden
    (provider-name
     &key
       (input *standard-input*)
       (input-file-descriptor
         (and (eq input *standard-input*)
              *api-key-input-file-descriptor*))
       (stream *standard-output*)
       note)
  "Clearly prompt for PROVIDER-NAME's API key and read it without terminal echo."
  (fresh-line stream)
  (api-key--write-prompt-span
   stream ':brand (format nil "╭─ ~A authentication" provider-name))
  (terpri stream)
  (api-key--write-prompt-span stream ':brand "│ ")
  (api-key--write-prompt-span
   stream ':dim
   (format nil "Paste the ~A API key below, then press Enter." provider-name))
  (terpri stream)
  (api-key--write-prompt-span stream ':brand "│ ")
  (api-key--write-prompt-span
   stream ':dim "Input is hidden. Nothing will appear while you type or paste.")
  (terpri stream)
  (when (non-empty-string-p note)
    (api-key--write-prompt-span stream ':brand "│ ")
    (api-key--write-prompt-span stream ':hint note)
    (terpri stream))
  (api-key--write-prompt-span stream ':brand "╰─ ")
  (api-key--write-prompt-span stream ':user "API key › ")
  (finish-output stream)
  (let ((saved-mode
          (api-key--hidden-input-mode input input-file-descriptor)))
    (unwind-protect
         (let ((value (read-line input nil nil)))
           (and value (api-key--strip-bracketed-paste value)))
      (when saved-mode
        (api-key--restore-input-mode saved-mode))
      (terpri stream)
      (finish-output stream))))


;;;; -- API-Key Credential Manager --

(defclass api-key-credential-manager (credential-manager)
  ()
  (:documentation "A credential manager for one persistent API-key source."))

(defmethod credential-manager-provider-label ((manager api-key-credential-manager))
  "Name the API-key provider in credential failures."
  (credential-source-label (credential-manager-primary-source manager)))

(defmethod credential-manager-credential-description
    ((manager api-key-credential-manager))
  "Describe static API-key credentials precisely."
  (declare (ignore manager))
  "API key")

(defmethod credential-manager-login-hint ((manager api-key-credential-manager))
  "Describe the command that stores this provider's API key."
  (format nil "run autolith auth ~A to enter it"
          (credential-manager-provider-label manager)))

(defmethod credential-manager-refreshable-p ((manager api-key-credential-manager))
  "Static provider API keys cannot refresh."
  (declare (ignore manager))
  nil)

(-> api-key-credential-manager-create
    (&key
     (:provider-name non-empty-string)
     (:pathname pathname))
    api-key-credential-manager)
(defun api-key-credential-manager-create (&key provider-name pathname)
  "Create an API-key manager backed by PROVIDER-NAME in PATHNAME."
  (let ((source
          (make-instance
           'api-key-credential-source
           :pathname pathname
           :provider-name provider-name)))
    (make-instance 'api-key-credential-manager
                   :primary-source source
                   :bootstrap-source source)))

(-> api-key-credential-manager-save-key
    (api-key-credential-manager non-empty-string)
    oauth-credentials)
(defun api-key-credential-manager-save-key (manager api-key)
  "Save API-KEY for MANAGER and return request-scoped credential metadata."
  (call-with-secret-use
   (lambda ()
     (let* ((source (credential-manager-primary-source manager))
            (credentials
              (make-instance
               'oauth-credentials
               :access-token api-key
               :refresh-token nil
               :id-token nil
               :account-id
               (format nil "api-key/~A"
                       (api-key--canonical-provider-name
                        (api-key-credential-source-provider-name source)))
               :expires-at nil
               :source-path (credential-source-pathname source))))
       (credential-source-save source credentials)))))

(-> api-key-credential-available-p (api-key-credential-manager) boolean)
(defun api-key-credential-available-p (manager)
  "Return true when MANAGER has a stored API key.

The probe uses the same request-scope secret accounting as a provider request and
never retains the resulting credential after this call."
  (handler-case
      (progn
        (call-with-credentials
         manager
         (lambda (credentials)
           (declare (ignore credentials))
           t))
        t)
    (credentials-unavailable () nil)))


;;;; -- Environment API-Key Credential Sources --

(defclass environment-api-key-credential-source (credential-source)
  ((environment-variable
    :initarg :environment-variable
    :reader environment-api-key-credential-source-environment-variable
    :type non-empty-string
    :documentation
    "The environment variable holding this provider's static API key.")
   (account-id
    :initarg :account-id
    :reader environment-api-key-credential-source-account-id
    :type non-empty-string
    :documentation
    "The synthetic account identifier pinned for this static API key."))
  (:documentation
   "A read-only adapter loading one static provider API key from the environment."))

(-> environment-api-key-credential-source--pathname (string) pathname)
(defun environment-api-key-credential-source--pathname (account-id)
  "Return the conventional reporting path for ACCOUNT-ID's environment source."
  (merge-pathnames
   (format nil "~A-auth.sexp" account-id)
   (merge-pathnames
    "autolith/"
    (environment-directory
     "XDG_STATE_HOME"
     (merge-pathnames ".local/state/" (user-homedir-pathname))))))

(defmethod credential-source-pathname
    ((source environment-api-key-credential-source))
  "Report a conventional key path because the environment has no pathname."
  (environment-api-key-credential-source--pathname
   (environment-api-key-credential-source-account-id source)))

(defmethod credential-source-label
    ((source environment-api-key-credential-source))
  "Name the environment source in user-visible failures."
  (format nil "the ~A environment variable"
          (environment-api-key-credential-source-environment-variable source)))

(defmethod credential-source-load
    ((source environment-api-key-credential-source))
  "Load SOURCE's API key from its environment variable, or return NIL."
  (let ((key (uiop:getenv
              (environment-api-key-credential-source-environment-variable
               source))))
    (when (non-empty-string-p key)
      (make-instance 'oauth-credentials
                     :access-token key
                     :refresh-token nil
                     :id-token nil
                     :account-id
                     (environment-api-key-credential-source-account-id source)
                     :expires-at nil
                     :source-path (credential-source-pathname source)))))

(defmethod credential-source-save
    ((source environment-api-key-credential-source)
     (credentials oauth-credentials))
  "Reject writes to the environment source."
  (declare (ignore credentials))
  (error 'authentication-error
         :message
         (format nil "The ~A environment source is read-only."
                 (environment-api-key-credential-source-environment-variable
                  source))))


;;;; -- Static API-Key Credential Manager --

(defclass static-api-key-credential-manager (api-key-credential-manager)
  ()
  (:documentation
   "A static API-key manager that prefers the current environment key."))

(defmethod credential-manager-load ((manager static-api-key-credential-manager))
  "Prefer the current environment key, then load the saved interactive key."
  (let ((environment
          (credential-source-load
           (credential-manager-bootstrap-source manager))))
    (credential-manager-accept-account
     manager
     (or environment
         (credential-source-load
          (credential-manager-primary-source manager))
         (error 'credentials-unavailable
                :message
                (format nil "No ~A API key is available; ~A."
                        (credential-manager-provider-label manager)
                        (credential-manager-login-hint manager))
                :searched-paths
                (list (credential-source-pathname
                       (credential-manager-primary-source manager))))))))

(-> api-key-credential-manager-persist-key
    (api-key-credential-manager non-empty-string)
    oauth-credentials)
(defun api-key-credential-manager-persist-key (manager key)
  "Save KEY to MANAGER's primary store using that store's credential shape."
  (let ((source (credential-manager-primary-source manager)))
    (if (typep source 'api-key-credential-source)
        (api-key-credential-manager-save-key manager key)
        (let ((bootstrap (credential-manager-bootstrap-source manager)))
          (credential-source-save
           source
           (make-instance
            'oauth-credentials
            :access-token key
            :refresh-token nil
            :id-token nil
            :account-id
            (if (typep bootstrap 'environment-api-key-credential-source)
                (environment-api-key-credential-source-account-id bootstrap)
                (string-downcase (credential-manager-provider-label manager)))
            :expires-at nil
            :source-path (credential-source-pathname source)))))))

(-> api-key-validate-probe (string function) null)
(defun api-key-validate-probe (label thunk)
  "Run THUNK and translate network rejection into an authentication error for LABEL."
  (handler-case
      (progn
        (funcall thunk)
        nil)
    (sb-sys:deadline-timeout ()
      (error 'authentication-error
             :message
             (format nil "The ~A API key validation exceeded its response deadline."
                     label)))
    (dexador.error:http-request-unauthorized ()
      (error 'authentication-error
             :message (format nil "~A rejected the entered API key." label)))
    (http-request-failed (condition)
      (error 'authentication-error
             :message (format nil
                              "The ~A API key validation failed (HTTP ~D)."
                              label
                              (response-status condition))))
    (error (condition)
      (if (typep condition 'authentication-error)
          (error condition)
          (error 'authentication-error
                 :message
                 (format nil
                         "The ~A API key could not be validated; check the network."
                         label))))))

(-> api-key-login
    (api-key-credential-manager &key
                                (:stream stream)
                                (:input stream)
                                (:input-file-descriptor (option integer))
                                (:validate (option function)))
    string)
(defun api-key-login
    (manager
     &key
       (stream *standard-output*)
       (input *standard-input*)
       (input-file-descriptor
         (and (eq input *standard-input*)
              *api-key-input-file-descriptor*))
       validate)
  "Prompt for MANAGER's API key, optionally validate it, and persist it."
  (call-with-secret-use
   (lambda ()
     (let* ((label (credential-manager-provider-label manager))
            (bootstrap (credential-manager-bootstrap-source manager))
            (note
              (when (typep bootstrap 'environment-api-key-credential-source)
                (format nil "~A overrides the stored key when set."
                        (environment-api-key-credential-source-environment-variable
                         bootstrap))))
            (key
              (string-trim
               '(#\Space #\Tab #\Newline #\Return)
               (or (api-key-read-hidden
                    label
                    :input input
                    :input-file-descriptor input-file-descriptor
                    :stream stream
                    :note note)
                   ""))))
       (unless (non-empty-string-p key)
         (error 'authentication-error
                :message (format nil "No ~A API key was entered." label)))
       (when validate
         (funcall validate key))
       (api-key-credential-manager-persist-key manager key)
       (format nil "~A authentication was saved by Autolith." label)))))
