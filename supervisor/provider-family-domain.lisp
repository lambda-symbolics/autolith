;;;; -- Shared Provider Family Domain --

(in-package #:autolith-supervisor)

(defparameter *family-domain-gemini-aliases*
  '(("auto" . "gemini-3-pro-preview")
    ("pro" . "gemini-3-pro-preview")
    ("flash" . "gemini-3-flash-preview")
    ("flash-lite" . "gemini-3.1-flash-lite")
    ("auto-gemini-3" . "gemini-3-pro-preview")
    ("auto-gemini-2.5" . "gemini-2.5-pro")
    ("gemini-auto" . "gemini-3-pro-preview")
    ("gemini-pro" . "gemini-3-pro-preview")
    ("gemini-flash" . "gemini-3-flash-preview"))
  "Code Assist aliases shared by both hosts.")

(serapeum:-> family-domain-gemini-model-name (string) string)
(defun family-domain-gemini-model-name (name)
  "Resolve NAME to a Code Assist wire model identifier."
  (or (rest (assoc name *family-domain-gemini-aliases* :test #'string=)) name))

(serapeum:-> family-domain--gemini-metadata ((or null string)) hash-table)
(defun family-domain--gemini-metadata (project)
  "Build Code Assist client metadata for PROJECT."
  (let ((metadata (cl-rfc8628:json-object "ideType" "IDE_UNSPECIFIED"
                                         "platform" "PLATFORM_UNSPECIFIED"
                                         "pluginType" "GEMINI")))
    (when project
      (setf (gethash "duetProject" metadata) project))
    metadata))

(serapeum:-> family-domain--gemini-operation-p (t) boolean)
(defun family-domain--gemini-operation-p (name)
  "Return true for a relative, unambiguous Code Assist operation name."
  (and (stringp name)
       (uiop:string-prefix-p "operations/" name)
       (> (length name) (length "operations/"))
       (not (search ".." name))
       (not (find #\? name))
       (not (find #\# name))
       t))

(serapeum:-> family-domain-gemini-setup
    (&key (:project (or null string)) (:rpc function) (:operation function)
     (:wait function) (:check function) (:fail function))
    (values string string))
(defun family-domain-gemini-setup (&key project rpc operation wait check fail)
  "Return effective project and tier after Code Assist setup.

RPC takes method, stage and request. OPERATION reads a validated operation name.
WAIT and CHECK enforce host cancellation and deadlines. FAIL must signal a host
condition; it takes a reason and stage, project or response keyword arguments.
The host owns credentials, HTTP response closure and committed setup state."
  (funcall check)
  (when (and project (plusp (length project)) (every #'digit-char-p project))
    (funcall fail ':numeric-gemini-project :stage ':load :project project))
  (let* ((loaded    (funcall rpc "loadCodeAssist" ':load
                             (cl-rfc8628:json-object
                              "cloudaicompanionProject" project
                              "metadata" (family-domain--gemini-metadata project))))
         (current   (gethash "currentTier" loaded))
         (paid      (gethash "paidTier" loaded))
         (effective (or (gethash "cloudaicompanionProject" loaded) project)))
    (funcall check)
    (if (hash-table-p current)
        (progn
          (unless (family-account-non-empty-string-p effective)
            (funcall fail ':gemini-project-required :stage ':load :response loaded))
          (values effective (or (and (hash-table-p paid) (gethash "id" paid))
                                (gethash "id" current) "standard-tier")))
        (let* ((tiers           (gethash "allowedTiers" loaded))
               (tier            (or (and (vectorp tiers)
                                         (find-if (lambda (entry)
                                                    (and (hash-table-p entry)
                                                         (gethash "isDefault" entry)))
                                                  tiers))
                                    (cl-rfc8628:json-object "id" "legacy-tier")))
               (tier-id         (or (gethash "id" tier) "standard-tier"))
               (onboard-project (unless (equal tier-id "free-tier") project))
               (onboard         (funcall rpc "onboardUser" ':onboard
                                         (cl-rfc8628:json-object
                                          "tierId" tier-id
                                          "cloudaicompanionProject" onboard-project
                                          "metadata" (family-domain--gemini-metadata onboard-project)))))
          (funcall check)
          (loop until (gethash "done" onboard)
                for name = (gethash "name" onboard)
                do (unless (family-domain--gemini-operation-p name)
                     (funcall fail ':invalid-gemini-operation :stage ':operation))
                   (funcall check)
                   (funcall wait 5)
                   (funcall check)
                   (setf onboard (funcall operation name))
                   (funcall check))
          (let* ((response  (gethash "response" onboard))
                 (companion (and (hash-table-p response)
                                 (gethash "cloudaicompanionProject" response)))
                 (result    (or (and (hash-table-p companion) (gethash "id" companion)) project)))
            (unless (family-account-non-empty-string-p result)
              (funcall fail ':gemini-project-required :stage ':onboard :response loaded))
            (values result tier-id))))))

(serapeum:-> family-domain-copilot-session
    (&key (:acquire function) (:now function) (:check function) (:fail function)) hash-table)
(defun family-domain-copilot-session (&key acquire now check fail)
  "Acquire and validate a request-local Auto session through host callbacks.

NOW returns Unix seconds. FAIL must signal a host condition for its reason."
  (funcall check)
  (let ((session (funcall acquire)))
    (funcall check)
    (unless (hash-table-p session)
      (funcall fail ':invalid-auto-session))
    (let ((expiry (gethash "expires_at" session)))
      (when (and (integerp expiry) (<= expiry (funcall now)))
        (funcall fail ':expired-auto-session)))
    session))

(serapeum:-> family-domain-copilot-model
    (&key (:session hash-table) (:catalog function) (:refresh function)
     (:vision-p boolean) (:preferred-model (or null string))
     (:check function) (:fail function)) (values list string))
(defun family-domain-copilot-model
    (&key session catalog refresh vision-p preferred-model check fail)
  "Return model specification and token, refreshing the trusted catalog once.

CATALOG and REFRESH return domain-pinned catalog documents from the host.
Only entitlement or model selection failures trigger a refresh. Host failures
and cancellation propagate. FAIL signals the final host selection condition."
  (labels ((select-model (document)
             (funcall check)
             (multiple-value-prog1
                 (cl-llm-provider-api:copilot-auto-session-model
                  session document :model-prefix "copilot/" :vision-p vision-p
                  :preferred-model preferred-model)
               (funcall check))))
    (funcall check)
    (handler-case (select-model (funcall catalog))
      (cl-llm-provider-api:provider-error ()
        (funcall check)
        (let ((document (funcall refresh)))
          (funcall check)
          (handler-case (select-model document)
            (cl-llm-provider-api:provider-error ()
              (funcall fail ':unusable-auto-session))))))))
