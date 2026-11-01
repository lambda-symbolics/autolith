(in-package #:autolith)

;;;; -- Command Permission Tests --

(-> test-command-permission-persistence () null)
(defun test-command-permission-persistence ()
  "Test exact command approvals persist, remain directory-scoped, and clear."
  (let* ((configuration (test-configuration))
         (root          (test-configuration-root configuration))
         (other         (merge-pathnames "other/" root))
         (state         (permissions-load configuration)))
    (unwind-protect
         (progn
           (test-assert
            (equal (configuration-permissions-path configuration)
                   (merge-pathnames "permissions.sexp"
                                    (config :state-root configuration)))
            "persistent command approvals live under the state root")
           (ensure-directories-exist other)
           (test-assert
            (not (permissions-allowed-p state "git status" root))
            "unknown commands are denied by persistent permission lookup")
           (let ((stale-a (permissions-load configuration))
                 (stale-b (permissions-load configuration)))
             (permissions-allow :configuration configuration
                                :state         stale-a
                                :command       "git diff"
                                :directory     root)
             (permissions-allow :configuration configuration
                                :state         stale-b
                                :command       "git show"
                                :directory     root)
             (test-assert (and (permissions-allowed-p stale-b "git diff" root)
                               (permissions-allowed-p stale-b "git show" root))
                          "stale approval callers merge fresh persisted permissions"))
           (permissions-allow :configuration configuration
                              :state         state
                              :command       "git status"
                              :directory     root)
           (test-assert (test-fixture-permissions-p
                         *platform*
                         (configuration-permissions-path configuration)
                         ':private-file)
                        "command permissions are private on disk")
           (let ((loaded (permissions-load configuration)))
             (test-assert (permissions-allowed-p loaded "git status" root)
                          "an exact command approval survives reload")
             (test-assert (not (permissions-allowed-p loaded "git status " root))
                          "command approvals match exact shell text")
             (test-assert (not (permissions-allowed-p loaded "git status" other))
                          "command approvals are scoped to their working directory")
             (permissions-clear configuration loaded)
             (test-assert
              (not (permissions-allowed-p
                    (permissions-load configuration) "git status" root))
              "clearing approvals persists an empty permission state")))
      (platform-delete-directory-tree *platform* root :validate t :if-does-not-exist ':ignore)))
  nil)

(-> test-command-permission-corruption () null)
(defun test-command-permission-corruption ()
  "Test corrupt permissions fail closed and explicit writes recover them."
  (with-test-configuration (configuration root)
    (let ((pathname (configuration-permissions-path configuration)))
      (dolist (source '("#.(error \"must not evaluate\")"
                        "(:permissions :version"
                        "(:permissions :version 99 :rules nil)"
                        "(:permissions :version 1 :rules nil :rules nil)"
                        "(:permissions :version 1 :rules ((:command \"\" :directory \"/\")))"
                        "(:permissions :version 1 :rules ((:command \"anything\" :directory \"/\" :command \"other\")))"))
        (snapshot-write-text pathname source)
        (let ((warned-p nil)
              (state    nil))
          (handler-bind
              ((permissions-load-warning
                 (lambda (condition)
                   (declare (ignore condition))
                   (setf warned-p t)
                   (muffle-warning))))
            (setf state (permissions-load configuration)))
          (test-assert warned-p "corrupt command permissions emit a warning")
          (test-assert (not (permissions-allowed-p state "anything" root))
                       "corrupt command permissions fail closed")
          (test-assert (string= source (uiop:read-file-string pathname))
                       "loading corrupt permissions preserves their bytes")
          (permissions-allow :configuration configuration :state state
                             :command "git status" :directory root)
          (test-assert (permissions-allowed-p (permissions-load configuration)
                                              "git status" root)
                       "an explicit approval replaces corrupt permission state")))
      (snapshot-write pathname
                      (list ':permissions ':version *permissions-version*
                            ':rules (list (list ':directory
                                                (permissions--directory-name root)
                                                ':command "git diff"))))
      (test-assert (permissions-allowed-p (permissions-load configuration)
                                          "git diff" root)
                   "permission records decode properties independently of key order")))
  nil)

(-> test-command-permission-write-failure () null)
(defun test-command-permission-write-failure ()
  "Test that a failed approval publication does not mutate its caller state."
  (let* ((configuration (test-configuration))
         (root          (test-configuration-root configuration))
         (state         (permissions-load configuration)))
    (unwind-protect
         (progn
           (ensure-directories-exist
            (merge-pathnames "permissions.sexp/"
                             (config :state-root configuration)))
           (test-assert
            (handler-case
                (progn
                  (permissions-allow :configuration configuration
                                     :state         state
                                     :command       "git status"
                                     :directory     root)
                  nil)
              (permissions-error () t))
            "a permission publication failure is reported")
           (test-assert (null (permission-state-rules state))
                        "a failed approval does not mutate caller state"))
      (platform-delete-directory-tree *platform* root :validate t :if-does-not-exist ':ignore)))
  nil)

(-> test-external-permission-persistence () null)
(defun test-external-permission-persistence ()
  "Exercise domain isolation, stale merging, reload, clear, and publication failure."
  (with-test-configuration (configuration root)
   (let* ((state (permissions-load configuration))
          (stale (permissions-load configuration))
          (other (merge-pathnames "other/" root)))
     (ensure-directories-exist other)
     (permissions-allow :configuration configuration :state state :command
                        "same-key" :directory root)
     (test-assert
      (not (permissions-allowed-p state "same-key" root :kind ':external-tool))
      "a shell approval cannot authorize an external call")
     (permissions-allow :configuration configuration :state stale :command
                        "same-key" :directory root :kind ':external-tool)
     (let ((loaded (permissions-load configuration)))
       (test-assert
        (and (permissions-allowed-p loaded "same-key" root)
             (permissions-allowed-p loaded "same-key" root :kind
                                    ':external-tool)
             (= 2 (length (permission-state-rules loaded))))
        "stale external approval merges with shell rules on disk")
       (test-assert
        (not
         (permissions-allowed-p loaded "same-key" other :kind ':external-tool))
        "external approvals are workspace-scoped")
       (permissions-clear configuration loaded)
       (test-assert
        (null (permission-state-rules (permissions-load configuration)))
        "the standard clear operation removes external rules too"))
     (test-call-with-function-replacements
      (list
       (list 'sexp-store:store-transact
             (lambda (&rest arguments)
               (declare (ignore arguments))
               (error 'store-error :message "forced failure" :pathname
                      (configuration-permissions-path configuration) :operation
                      ':write))))
      (lambda ()
        (test-assert
         (handler-case
          (progn
           (permissions-allow :configuration configuration :state state
                              :command "failed-key" :directory root :kind
                              ':external-tool)
           nil)
          (permissions-error nil t))
         "external publication failure is a typed permission failure")
        (test-assert
         (not
          (permissions-allowed-p state "failed-key" root :kind
                                 ':external-tool))
         "failed publication never grants an external approval")))))
  nil)

(-> test-external-tool-permission-modes () null)
(defun test-external-tool-permission-modes ()
  "Exercise external calls through shared modes, picker choices, and exact rules."
  (with-test-configuration (configuration root)
    (let* ((tool           (make-instance 'tool :namespace "service" :name "lookup"
                                          :description "Read one service record."
                                          :parameters  (json-object "type" "object")))
           (arguments      (json-object "record" "one"))
           (state          (permissions-load configuration))
           (application    (make-instance 'application :configuration configuration
                                          :permission-state           state
                                          :ui                         (terminal-ui-create
                                                                       :terminal (make-instance 'recording-terminal))
                                          :provider                   (make-instance 'rlm-inference-test-provider
                                                                                     :results nil)))
           (choice         "once")
           (pick-count     0)
           (classify-count 0)
           (decision       ':full-access))
      (setf (application-input-controller application)
            (make-instance 'application-input-controller :application application))
      (test-call-with-function-replacements
       (list
        (list 'application--interactive-command-approval-p
              (lambda (application) (declare (ignore application)) t))
        (list 'application-input-controller-call-with-reader-paused
              (lambda (controller function)
                (declare (ignore controller)) (funcall function)))
        (list 'application-present
              (lambda (&rest arguments) (declare (ignore arguments)) nil))
        (list 'terminal-ui-select
              (lambda (ui &key title items resize-callback status-kind)
                (declare (ignore ui title resize-callback))
                (incf pick-count)
                (test-assert
                 (and (eq status-kind ':permission)
                      (equal (mapcar (lambda (item) (getf item :name)) items)
                             '("auto" "once" "always" "full" "deny")))
                 "external calls offer normal permission choices without sandbox")
                choice))
        (list 'permissions-model-classify-command
              (lambda (request directory &key provider configuration
                                             sandbox-available-p user-instructions)
                (declare (ignore directory provider configuration user-instructions))
                (incf classify-count)
                (test-assert (and (not sandbox-available-p)
                                  (search "External tool call" request)
                                  (search "one" request))
                             "auto classifies the external identity and arguments without sandbox")
                (values decision "test policy"))))
       (lambda ()
         (test-assert (eq (application-authorize-tool application tool arguments) ':allow)
                      "once permits an external call")
         (test-assert (eq (application-permission-mode application) ':ask)
                      "once does not change session policy")
         (dolist (rejected '("deny" "sandbox" "allow" nil))
           (setf choice rejected)
           (test-assert (eq (application-authorize-tool application tool arguments) ':deny)
                        "cancelled, stale, and sandbox choices deny external calls"))
         (setf choice "always")
         (test-assert (eq (application-authorize-tool application tool arguments) ':allow)
                      "always persists the exact external call")
         (setf (application-permission-state application) (permissions-load configuration))
         (let ((before pick-count))
           (test-assert (eq (application-authorize-tool application tool arguments) ':allow)
                        "a remembered external rule survives reload")
           (test-assert (= before pick-count) "remembered external calls do not prompt"))
         (test-assert (not (search "record" (uiop:read-file-string
                                              (configuration-permissions-path configuration))))
                      "external permission files contain hashes, not argument payloads")
         (setf arguments (json-object "record" "one" "extra" t)
               choice "deny")
         (test-assert (eq (application-authorize-tool application tool arguments) ':deny)
                      "changed arguments require a new grant")
         (setf choice "full")
         (test-assert (eq (application-authorize-tool application tool arguments) ':allow)
                      "full grants session-wide external access")
         (let ((before pick-count))
           (test-assert (and (eq (application-authorize-command application "anything" root)
                                ':full-access)
                             (eq (application-authorize-tool application tool arguments) ':allow)
                             (= before pick-count))
                        "shell and external calls share the same full-access session mode"))
         (setf (application-permission-mode application) ':ask
               choice "auto")
         (test-assert (and (eq (application-authorize-tool application tool arguments) ':allow)
                           (eq (application-permission-mode application) ':auto))
                      "auto selects the shared session policy")
         (let ((before     pick-count)
               (classified classify-count))
           (test-assert (and (eq (application-authorize-tool application tool arguments) ':allow)
                             (= before pick-count)
                             (= classified classify-count))
                        "automatic external decisions reuse the normal classification cache"))
         (clrhash (application-command-classifications application))
         (setf decision ':sandboxed)
         (test-assert (eq (application-authorize-tool application tool arguments) ':deny)
                      "a classifier cannot grant a remote call a local sandbox")
         (setf (application-permission-mode application) ':sandboxed choice "deny")
         (test-assert (eq (application-authorize-tool application tool arguments) ':deny)
                      "local sandbox mode requires separate external approval")))))
  nil)

(-> test-command-classification-cache-bounds () null)
(defun test-command-classification-cache-bounds ()
  "Reuse decisions under one instruction and drop them when the instruction changes."
  (with-test-configuration (configuration root)
    (let ((application    (make-instance 'application :configuration configuration
                                         :permission-state           (permissions-load configuration)
                                         :provider                   (make-instance 'rlm-inference-test-provider
                                                                                    :results nil)))
          (instructions   "Build the project.")
          (classify-count 0))
      (test-call-with-function-replacements
       (list (list 'application--current-user-instructions
                   (lambda (application) (declare (ignore application)) instructions))
             (list 'permissions-model-classify-command
                   (lambda (command directory &key provider configuration
                                                   sandbox-available-p user-instructions)
                     (declare (ignore command directory provider configuration
                                      sandbox-available-p user-instructions))
                     (incf classify-count)
                     (values ':sandboxed "test policy"))))
       (lambda ()
         (flet ((classify (command)
                  (application--model-command-permission application command root
                                                         :sandbox-available-p t)))
           (classify "make")
           (classify "make")
           (test-assert (= 1 classify-count) "one instruction reuses a command's decision")
           (setf instructions "Run the tests.")
           (classify "make")
           (test-assert (= 2 classify-count) "a new instruction asks again")
           (test-assert (= 1 (hash-table-count (application-command-classifications application)))
                        "decisions made under earlier instructions leave the cache")
           (let ((*application-command-classification-limit* 4))
             (dotimes (index 10)
               (classify (format nil "command-~D" index)))
             (test-assert (<= (hash-table-count (application-command-classifications application)) 4)
                          "one instruction cannot grow the cache past its limit")))))))
  nil)

(-> test-external-tool-permission-keys () null)
(defun test-external-tool-permission-keys ()
  "Verify exact typed identity and order-independent JSON object keys."
  (let ((first  (make-instance 'tool :namespace "service" :name "first"))
        (second (make-instance 'tool :namespace "service" :name "second")))
    (test-assert
     (string=
      (application--tool-permission-key first (json-object "a" 1 "b" 2))
      (application--tool-permission-key first (json-object "b" 2 "a" 1)))
     "object field order does not invalidate an exact external rule")
    (test-assert
     (not
      (string= (application--tool-permission-key first (json-object))
               (application--tool-permission-key second (json-object))))
     "tool identity is part of the remembered external rule")
    (let ((values (list (json-object) #() nil (json-false) "" 0)))
      (test-assert
       (= (length values)
          (length
           (remove-duplicates
            (mapcar
             (lambda (value)
               (application--tool-permission-key first
                (json-object "value" value)))
             values)
            :test #'string=)))
       "external rules distinguish empty objects, arrays, null, booleans, strings, and numbers")))
  nil)

(-> test-external-tool-permission-failures () null)
(defun test-external-tool-permission-failures ()
  "Fail closed without an approval owner or after remembered-rule publication fails."
  (with-test-configuration (configuration root)
   (let* ((tool
           (make-instance 'tool :namespace "service" :name "lookup"
                          :description "Read one service record."))
          (arguments (json-object))
          (application
           (make-instance 'application :configuration configuration :ui nil))
          (key       (application--tool-permission-key tool arguments)))
     (test-assert
      (eq (application-authorize-tool application tool arguments) ':deny)
      "a headless session without a classifier cannot silently grant an external call")
     (setf (application-permission-mode application) ':auto)
     (test-assert
      (eq (application-authorize-tool application tool arguments) ':deny)
      "auto denies when no model provider can classify the external call")
     (ensure-directories-exist
      (merge-pathnames "permissions.sexp/" (config :state-root configuration)))
     (test-assert
      (handler-case
       (progn
        (application--apply-command-authorization-choice application key root
                                                         :choice "always"
                                                         :kind   ':external-tool)
        nil)
       (permissions-error nil t))
      "remembered external approval cannot succeed after persistence failure")
     (test-assert
      (not
       (permissions-allowed-p (application-permission-state application) key
                              root :kind ':external-tool))
      "failed remembered approval never authorizes a later external call")))
  nil)
