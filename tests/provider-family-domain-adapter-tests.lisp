(in-package #:autolith)

;;;; -- Legacy Shared Domain Adapter Checks --

(-> family-domain-test--credentials () oauth-credentials)
(defun family-domain-test--credentials ()
  "Return credentials used only by isolated transport fixtures."
  (make-instance 'oauth-credentials :access-token "fixture-token"
                                    :expires-at   (+ (get-universal-time) 3600)))

(-> family-domain-test--gemini
    (&key (:project (option string)) (:rpc function) (:operation function)
     (:wait function) (:check function) (:fail function)) (values string string))
(defun family-domain-test--gemini (&key project rpc operation wait check fail)
  "Run a shared setup fixture through the real legacy Code Assist adapter."
  (declare (ignore check))
  (let ((provider (gemini-code-assist-test--provider)))
    (setf (gemini-code-assist-provider-project provider) project)
    (test-call-with-function-replacements
     (list (list 'gemini-code-assist--post
                 (lambda (provider credentials &key method stage request)
                   (declare (ignore provider credentials))
                   (funcall rpc method stage request)))
           (list 'gemini-code-assist--operation
                 (lambda (provider credentials name)
                   (declare (ignore provider credentials))
                   (funcall operation name)))
           (list 'gemini-code-assist--wait wait))
     (lambda ()
       (handler-case
           (progn
             (gemini-code-assist-ensure-setup provider (family-domain-test--credentials))
             (values (gemini-code-assist-provider-project provider)
                     (gemini-code-assist-provider-tier provider)))
         (gemini-code-assist-invalid-project ()
           (funcall fail ':numeric-gemini-project))
         (gemini-code-assist-project-required ()
           (funcall fail ':gemini-project-required))
         (gemini-code-assist-setup-error ()
           (funcall fail ':invalid-gemini-operation)))))))

(-> family-domain-test--auto-provider () copilot-auto-provider)
(defun family-domain-test--auto-provider ()
  "Return a legacy Auto adapter without reading a real account."
  (let ((configuration (configuration-copy (test-configuration) :model                   "copilot/auto"
                                                                  :provider-validation-p nil)))
    (make-instance 'copilot-auto-provider :configuration     configuration
                                         :credential-manager (credential-manager-create configuration)
                                         :session-id         "fixture-session")))

(-> family-domain-test--session
    (&key (:acquire function) (:now function) (:check function) (:fail function)) json-object)
(defun family-domain-test--session (&key acquire now check fail)
  "Run a shared session fixture through the real legacy Auto adapter."
  (declare (ignore now check))
  (test-call-with-function-replacements
   (list (list 'copilot-auto--acquire-session
               (lambda (provider credentials)
                 (declare (ignore provider credentials))
                 (funcall acquire))))
   (lambda ()
     (handler-case
         (copilot-auto--session (family-domain-test--auto-provider)
                               (family-domain-test--credentials))
       (configuration-error ()
         (funcall fail ':expired-auto-session))))))

(-> family-domain-test--model
    (&key (:session json-object) (:catalog function) (:refresh function)
     (:vision-p boolean) (:preferred-model (option string)) (:check function) (:fail function))
    (values list string))
(defun family-domain-test--model
    (&key session catalog refresh vision-p preferred-model check fail)
  "Run shared selection fixtures through the real legacy Auto adapter."
  (declare (ignore check))
  (let ((provider  (family-domain-test--auto-provider))
        (refreshed nil)
        (document  nil))
    (setf (copilot-auto-provider-preferred-model provider) preferred-model)
    (test-call-with-function-replacements
     (list (list 'copilot-auto--catalog
                 (lambda (configuration)
                   (declare (ignore configuration))
                   (if refreshed document (funcall catalog))))
           (list 'copilot--fetch-models
                 (lambda (configuration &key enable-p)
                   (declare (ignore configuration enable-p))
                   (setf document (funcall refresh)
                         refreshed t))))
     (lambda ()
       (handler-case (copilot-auto--model provider session :vision-p vision-p)
         (configuration-error ()
           (funcall fail ':unusable-auto-session)))))))

(-> test-provider-family-domain-adapters () null)
(defun test-provider-family-domain-adapters ()
  "Run the shared fixture matrix through the legacy Gemini and Auto adapters."
  (with-test-configuration (configuration)
    (declare (ignore configuration))
    (with-test-environment (("GOOGLE_CLOUD_PROJECT" nil)
                            ("GOOGLE_CLOUD_PROJECT_ID" nil))
      (test-assert
       (plusp (autolith-supervisor:run-family-domain-fixtures
               :gemini  #'family-domain-test--gemini
               :session #'family-domain-test--session
               :model   #'family-domain-test--model))
       "Legacy adapters pass the common domain fixtures")))
  nil)
