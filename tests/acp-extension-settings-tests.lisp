(in-package #:autolith)

;;;; -- Settings and Model Wire Tests --

(-> acp-settings-test--row (vector string) hash-table)
(defun acp-settings-test--row (rows name)
  "Find the complete settings row named NAME in ROWS."
  (or (find name rows :test #'equal :key (lambda (row) (gethash "name" row)))
      (error "Missing setting ~A." name)))

(-> test-acp-extension-settings-types-and-mutation () null)
(defun test-acp-extension-settings-types-and-mutation ()
  "Read typed settings, apply booleans, and reject invalid or fixed settings over ACP."
  (with-test-configuration (configuration root)
    (acp-session-test--call-with-client
     configuration
     (lambda (service client)
       (let* ((identifier (agentcomms:client-new-session client (namestring root)))
              (session (acp-service--session service identifier))
              (application (acp-session-application session)))
         (labels ((request (method &rest fields)
                    "Send one exact-session extension request."
                    (agentcomms:client-agent-request
                     client method (apply #'agentcomms:json-object "sessionId" identifier fields))))
           (let* ((response (request "_autolith/settings"))
                  (rows (gethash "items" (gethash "value" response)))
                  (boolean (acp-settings-test--row rows "hurry-up-p")))
             (test-assert (equal "ok" (gethash "outcome" response)) "settings list succeeds")
             (test-assert (vectorp rows) "settings are a JSON array")
             (test-assert (equal "choice" (gethash "type" (acp-settings-test--row rows "model")))
                          "dynamic model choices are editable choices")
             (test-assert (equal "derived" (gethash "type" (acp-settings-test--row rows "context-window")))
                          "derived numeric values have a derived editor type")
             (test-assert (argo:json-false-p (gethash "adjustable" (acp-settings-test--row rows "working-directory")))
                          "process settings are fixed")
             (test-assert (argo:json-false-p (gethash "value" boolean)) "boolean defaults preserve false")
             (test-assert (find (argo:json-false) (gethash "options" boolean)) "boolean options include false"))
           (dolist (enabled (list t (argo:json-false)))
             (let* ((response (request "_autolith/set-setting" "name" "hurry-up-p" "value" enabled))
                    (row (gethash "value" response)))
               (test-assert (equal "ok" (gethash "outcome" response)) "boolean setting mutation succeeds")
               (test-assert (equal "hurry-up-p" (gethash "name" row)) "the setter returns its complete row")
               (test-assert (equal "session" (gethash "scope" row)) "the updated row retains its scope")
               (test-assert (eql enabled (gethash "value" row)) "the updated boolean has its requested wire value")
               (test-assert (eq (eq enabled t) (application-hurry-up-p application))
                            "the runtime side effect matches the setting")))
           (let ((fixed (request "_autolith/set-setting" "name" "working-directory" "value" "/")))
             (test-assert (equal "unsupported" (gethash "outcome" fixed)) "fixed settings are rejected"))
           (let ((model (config :model (application-configuration application))))
             (let ((invalid (request "_autolith/set-setting" "name" "model" "value" "not-a-registered-model")))
               (test-assert (equal "condition" (gethash "outcome" invalid)) "invalid models fail validation")
               (test-assert (equal model (config :model (application-configuration application)))
                            "failed model validation does not change the active model"))))))))
  nil)

(-> test-acp-extension-model-verification () null)
(defun test-acp-extension-model-verification ()
  "Distinguish stored credentials from an observed successful provider request."
  (with-test-configuration (configuration root)
    (acp-session-test--call-with-client
     configuration
     (lambda (service client)
       (let* ((identifier (agentcomms:client-new-session client (namestring root)))
              (session (acp-service--session service identifier))
              (model (config :model (application-configuration (acp-session-application session)))))
         (test-call-with-function-replacements
          (list (list 'acp-extension--credential-stored-p
                      (lambda (configuration registration)
                        (declare (ignore configuration registration)) t)))
          (lambda ()
            (labels ((row ()
                       "Read the current model's negotiated row without any credential files."
                       (let* ((response (agentcomms:client-agent-request
                                         client "_autolith/models"
                                         (agentcomms:json-object "sessionId" identifier)))
                              (items (gethash "items" (gethash "value" response))))
                         (find model items :test #'equal :key (lambda (item) (gethash "id" item))))))
              (let ((before (row)))
                (test-assert (eq t (gethash "credentialStored" before)) "stored presence is independent")
                (test-assert (argo:json-false-p (gethash "recentlyVerified" before))
                             "stored credentials alone are not verified"))
              (agent-observer-status (acp-observer-create session) ':provider-request-completed
                                     (list :usage '(("input_tokens" 3))))
              (test-assert (eq t (gethash "recentlyVerified" (row)))
                           "successful provider completion marks this model verified"))))))))
  nil)
