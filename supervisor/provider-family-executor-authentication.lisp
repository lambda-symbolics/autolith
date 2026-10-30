;;;; -- Owned Authentication Transport --

(in-package #:autolith-supervisor)

(defvar *provider-family-active-executor* nil
  "The executor owning request-local credential renewal.")

(defvar *provider-family-active-state* nil
  "The same cancellation and strict-close state used for inference.")

(defvar *provider-family-active-invocation* nil
  "The invocation whose deadline also bounds authentication.")

(declaim (ftype (function (string &key (:method keyword) (:headers list) (:content t)
                           (:deadline-seconds real)) (values t t t &optional)) provider-family--auth-open))

(defun provider-family--auth-open (url &key (method ':post) headers content (deadline-seconds 60))
  "Open one unpooled authentication transport without retaining an HTTP error condition."
  (dexador:request url :method method :headers headers :content content :want-stream t
                       :keep-alive nil :use-connection-pool nil :max-redirects 0
                       :connect-timeout (min 30 deadline-seconds) :read-timeout deadline-seconds))

(defparameter *provider-family-auth-open-function* #'provider-family--auth-open
  "Trusted authentication transport hook. Tests use local HTTP endpoints only.")

(declaim (ftype (function (string &key (:method keyword) (:headers list) (:content t))
                         (values string integer t &optional)) provider-family--auth-request))

(defun provider-family--auth-request (url &key (method ':post) headers content)
  "Own authentication HTTP through the invocation's cancellation and close barrier."
  (let* ((executor   *provider-family-active-executor*)
         (state      *provider-family-active-state*)
         (invocation *provider-family-active-invocation*)
         (clock      (chatgpt-executor-clock executor))
         (closer     (chatgpt-executor-closer executor)))
    (unwind-protect
         (cl-llm-provider-api:provider-call-with-response-deadline
          (min 60 (- (model-invocation-deadline invocation) (funcall clock)))
          (lambda ()
            (multiple-value-bind (stream status response-headers)
                (provider-http--open
                 state
                 (lambda ()
                   (provider-http--check-invocation state invocation clock)
                   (provider-http--http-values
                    (lambda ()
                      (funcall *provider-family-auth-open-function* url :method method
                               :headers headers :content content
                               :deadline-seconds                                (min 60 (- (model-invocation-deadline invocation)
                                                                                           (funcall clock)))))))
                 closer)
              (unless (integerp status)
                (error 'provider-executor-error :reason ':invalid-authentication-status))
              (values (provider-family--response-text stream) status response-headers))))
      (provider-http--close state closer))))

(defmethod provider-executor-with-credentials ((account provider-account)
                                               &key executor state invocation function force-refresh)
  "Bind explicit trusted settings and own authentication transport before credential loading."
  (let* ((*provider-family-active-executor*        executor)
         (*provider-family-active-state*           state)
         (*provider-family-active-invocation*      invocation)
         (*provider-account-copilot-json-function* #'provider-family--copilot-auth-json)
         (*provider-account-active-configuration*  (provider-account-configuration account))
         (*grok-oauth-issuer*                      (or (and *provider-account-active-configuration*
                                                            (provider-account-config ':grok-issuer
                                                                                     *provider-account-active-configuration*))
                                                       *grok-oauth-issuer*))
         (*grok-oauth-client-id*                   (or (and *provider-account-active-configuration*
                                                            (provider-account-config ':grok-client-id
                                                                                     *provider-account-active-configuration*))
                                                       *grok-oauth-client-id*)))
    (cl-rfc8628:call-with-credentials (provider-account-credential-manager account)
                                      function :force-refresh force-refresh)))

(defmethod cl-rfc8628:credential-manager-refresh-request :around
    ((manager grok-credential-manager) &key url headers content)
  "Use the invocation's owned transport for xAI refresh."
  (if *provider-family-active-state*
      (provider-family--auth-request url :headers headers :content content)
      (call-next-method)))

(defmethod cl-rfc8628:credential-manager-refresh-request :around
    ((manager gemini-credential-manager) &key url headers content)
  "Use the invocation's owned transport for Google refresh."
  (if *provider-family-active-state*
      (provider-family--auth-request url :headers headers :content content)
      (call-next-method)))

(defmethod cl-rfc8628:credential-manager-refresh-request :around
    ((manager nous-credential-manager) &key url headers content)
  "Keep Nous refresh serialized and own its actual HTTP stream."
  (if (and *provider-family-active-state*
           (not (nous-credential-manager-refresh-request-function manager)))
      (provider-family--auth-request url :headers headers :content content)
      (call-next-method)))

(declaim (ftype (function (string string &key (:headers list)) hash-table)
                provider-family--copilot-auth-json))

(defun provider-family--copilot-auth-json (url token &key headers)
  "Read Copilot authentication JSON with the same owned transport as inference."
  (multiple-value-bind (body status)
      (provider-family--auth-request url :method ':get
                                     :headers    (append (copilot--headers token) headers))
    (unless (<= 200 status 299)
      (error 'authentication-error :message "GitHub Copilot rejected token renewal."))
    (let ((document (handler-case (cl-rfc8628:json-decode body)
                      (error () (error 'authentication-error :message "Invalid Copilot token response.")))))
      (unless (hash-table-p document)
        (error 'authentication-error :message "Invalid Copilot token response."))
      document)))
