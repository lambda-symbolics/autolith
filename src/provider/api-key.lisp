(in-package #:autolith)

;;;; -- Interactive API-Key Input --
(defparameter *api-key-input-echo-disabled-p* nil
  "Whether the dynamically bound API-key input transport already suppresses echo.")

(defparameter *api-key-input-file-descriptor* nil
  "The known descriptor for dynamically bound standard API-key input.")

(defparameter *api-key-output-styled-p* nil
  "Whether API-key prompts use trusted semantic terminal styling.")

(-> api-key--input-file-descriptor (stream (option integer)) (option integer))
(defun api-key--input-file-descriptor (input configured)
  "Return INPUT's configured or direct file descriptor."
  (or configured
      (handler-case (sb-sys:fd-stream-fd input)
        (type-error ()
          nil))))

(-> api-key--read-concealed (stream stream (option integer)) (option string))
(defun api-key--read-concealed (input output configured-descriptor)
  "Read one API-key line from INPUT through clinedi with terminal echo off.

The read fails closed: without a known descriptor, input is read only when the
bound transport already suppresses echo, and a terminal that cannot be
concealed signals AUTHENTICATION-ERROR before anything is read."
  (let ((descriptor (api-key--input-file-descriptor input configured-descriptor)))
    (when (and (null descriptor) (not *api-key-input-echo-disabled-p*))
      (error 'authentication-error
             :message
             "Could not identify the input descriptor for API-key entry; no key was read."))
    (handler-case
        (clinedi:terminal-read-concealed-line
         (stream-terminal-create :input-stream          input
                                 :output-stream         output
                                 :input-file-descriptor (or descriptor -1)))
      (clinedi:terminal-error (condition)
        (if (eq (clinedi:terminal-error-operation condition) ':conceal)
            (error 'authentication-error
                   :message
                   "Could not disable terminal echo for API-key entry; no key was read.")
            (error condition))))))

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
  (unwind-protect
       (api-key--read-concealed input stream input-file-descriptor)
    (terpri stream)
    (finish-output stream)))

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
            :access-token  key
            :refresh-token nil
            :id-token      nil
            :account-id
            (if (typep bootstrap 'environment-api-key-credential-source)
                (cl-rfc8628:environment-credential-source-account-id bootstrap)
                (string-downcase (credential-manager-provider-label manager)))
            :expires-at    nil
            :source-path   (credential-source-pathname source)))))))

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
     (let* ((label     (credential-manager-provider-label manager))
            (bootstrap (credential-manager-bootstrap-source manager))
            (note
              (when (typep bootstrap 'environment-api-key-credential-source)
                (format nil "~A overrides the stored key when set."
                        (cl-rfc8628:environment-credential-source-environment-variable
                         bootstrap))))
            (key
              (string-trim
               '(#\Space #\Tab #\Newline #\Return)
               (or (api-key-read-hidden
                    label
                    :input                 input
                    :input-file-descriptor input-file-descriptor
                    :stream                stream
                    :note                  note)
                   ""))))
       (unless (non-empty-string-p key)
         (error 'authentication-error
                :message (format nil "No ~A API key was entered." label)))
       (when validate
         (funcall validate key))
       (api-key-credential-manager-persist-key manager key)
       (format nil "~A authentication was saved by Autolith." label)))))
