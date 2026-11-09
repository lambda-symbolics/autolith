(in-package #:autolith)

;;;; -- Static API-Key Provider Definitions --

(-> api-key-validate-models-endpoint (string string list) null)
(defun api-key-validate-models-endpoint (label url headers)
  "Probe the model list at URL with HEADERS, signaling LABEL's authentication error on rejection."
  (api-key-validate-probe
   label
   (lambda ()
     (provider-call-with-response-deadline
      60
      (lambda ()
        (dexador:get url
                     :headers         headers
                     :force-string    t
                     :keep-alive      nil
                     :connect-timeout 30
                     :read-timeout    60))))))

(defmacro define-static-api-key-provider
    (name &key display-name environment-variable provider-name
          source-class source-path login-hint
          models-endpoint key-headers validate-function)
  "Define the classes, factory, key validation, and login of one static API-key provider.

NAME supplies the provider prefix used by the public class and function names.
SOURCE-CLASS and SOURCE-PATH customize the persistent credential source; when
omitted, the shared API-key store is used. MODELS-ENDPOINT, a form evaluated at
validation time, and KEY-HEADERS, a form evaluating to a function from a key to
request headers, define NAME-VALIDATE-API-KEY as a probe of that model list.
VALIDATE-FUNCTION instead names an existing probe. NAME-API-KEY-LOGIN prompts
for the key, validates it when a probe exists, and saves it."
  (let* ((prefix            (string-upcase (string name)))
         (name-string       (string-downcase (string name)))
         (display-name      (or display-name prefix))
         (provider-name     (or provider-name name-string))
         (account-label     (intern (format nil "*~A-ACCOUNT-LABEL*" prefix)))
         (environment-var   (intern (format nil "*~A-ENVIRONMENT-VARIABLE*" prefix)))
         (environment-class (intern (format nil "~A-ENVIRONMENT-CREDENTIAL-SOURCE" prefix)))
         (manager-class     (intern (format nil "~A-CREDENTIAL-MANAGER" prefix)))
         (create-function   (intern (format nil "~A-CREDENTIAL-MANAGER-CREATE" prefix)))
         (login-function    (intern (format nil "~A-API-KEY-LOGIN" prefix)))
         (validate          (or validate-function
                                (and models-endpoint
                                     (intern (format nil "~A-VALIDATE-API-KEY" prefix))))))
    `(progn
       (defparameter ,account-label ,name-string
         ,(format nil "Synthetic account identifier for ~A API keys." display-name))
       (defparameter ,environment-var ,environment-variable
         ,(format nil "The environment variable holding the ~A account API key." display-name))

       (defclass ,environment-class (environment-api-key-credential-source)
         ()
         (:default-initargs
          :environment-variable ,environment-var
          :account-id (format nil "api-key/~A" ,account-label))
         (:documentation
          ,(format nil "Read-only source for the ~A API key from the environment." display-name)))

       (defclass ,manager-class (static-api-key-credential-manager)
         ()
         (:documentation
          ,(format nil "Credential manager for ~A API keys." display-name)))

       (defmethod credential-manager-provider-label ((manager ,manager-class))
         ,(format nil "Name ~A in user-visible credential failures." display-name)
         (declare (ignore manager))
         ,display-name)

       ,@(when login-hint
           `((defmethod credential-manager-login-hint ((manager ,manager-class))
               ,(format nil "Return the ~A login command for ~A credential errors." display-name display-name)
               (declare (ignore manager))
               ,login-hint)))

       (-> ,create-function (configuration) ,manager-class)
       (defun ,create-function (configuration)
         ,(format nil "Create the ~A credential manager with CONFIGURATION's private paths." display-name)
         (make-instance ',manager-class
                        :primary-source
                        (make-instance ',(or source-class 'api-key-credential-source)
                                       :pathname ,(or source-path
                                                     `(configuration-api-keys-path configuration))
                                       ,@(unless source-class
                                           `(:provider-name ,provider-name)))
                        :bootstrap-source
                        (make-instance ',environment-class)))

       ,@(when models-endpoint
           `((-> ,validate (string) null)
             (defun ,validate (key)
               ,(format nil "Probe the ~A model list with KEY, signaling on rejection." display-name)
               (api-key-validate-models-endpoint ,display-name ,models-endpoint
                                                 (funcall ,key-headers key)))))

       (-> ,login-function
           (,manager-class &key (:stream stream) (:input stream)
                           (:input-file-descriptor (option integer)))
           string)
       (defun ,login-function
           (manager &key (stream *standard-output*)
                         (input *standard-input*)
                         (input-file-descriptor
                          (and (eq input *standard-input*)
                               *api-key-input-file-descriptor*)))
         ,(format nil "Prompt for~:[~; and validate~] the ~A API key and save it to MANAGER's store."
                  validate display-name)
         (api-key-login manager
                        :stream                stream
                        :input                 input
                        :input-file-descriptor input-file-descriptor
                        ,@(when validate
                            `(:validate #',validate)))))))
