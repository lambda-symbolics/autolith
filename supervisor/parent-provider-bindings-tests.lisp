;;;; -- Parent Extension Binding and Cancellation Checks --

(in-package #:autolith-supervisor)

(defclass parent-provider-blocking-fixture (parent-provider-fixture) ()
  (:documentation "Trusted callback with observable unwind for cancellation checks."))

(defmethod cl-llm-provider-api:provider-stream-turn
    ((provider parent-provider-blocking-fixture) projection
     &key tool-namespaces event-callback goal-context compaction-p)
  "Block until cancellation and expose completion of the actual callback unwind."
  (declare (ignore projection tool-namespaces event-callback goal-context compaction-p))
  (let* ((configuration (parent-provider-fixture-configuration provider))
         (lock          (getf configuration :lock))
         (state         (getf configuration :state)))
    (unwind-protect
        (progn
          (bordeaux-threads:with-lock-held (lock) (setf (first state) t))
          (sleep 60)
          (make-instance 'cl-llm-provider-api:provider-result))
      (bordeaux-threads:with-lock-held (lock) (setf (second state) t)))))

(serapeum/types:-> run-parent-provider-binding-tests (pathname) integer)
(defun run-parent-provider-binding-tests (temporary)
  "Check model selection, persistent discovery and strict custom callback cancellation."
  (let* ((registry                             (make-parent-provider-extension-registry))
         (*parent-provider-extension-registry* registry)
         (factory                              (lambda (configuration &key reasoning-summaries-p)
                                                 (declare (ignore reasoning-summaries-p))
                                                 (make-instance 'parent-provider-fixture :configuration configuration)))
         (checks                               0))
    (labels ((check (value)
               "Count a contract check."
               (incf checks)
               (assert value))

             (reason (function)
               "Read a fixed extension failure category."
               (handler-case (progn (funcall function) nil)
                 (parent-provider-extension-error (condition)
                   (parent-provider-extension-error-reason condition)))))
      (register-parent-provider "alpha" :models '("shared" "team/model") :factory factory)
      (register-parent-provider "beta" :models '("alpha/shared") :factory factory)
      (check (equal (subseq (multiple-value-list
                             (parent-provider-resolve-model registry "alpha/shared" :provider-name "beta")) 0 2)
                    '("alpha/shared" "beta")))
      (check (equal (subseq (multiple-value-list
                             (parent-provider-resolve-model registry "alpha/team/model" :provider-name "ALPHA")) 0 2)
                    '("team/model" "alpha")))
      (check (eq ':ambiguous-model (reason (lambda () (parent-provider-resolve-model registry "alpha/shared")))))
      (dolist (provider-name '(nil ""))
        (dolist (case '(("gpt-6.1-sol" "gpt-6.1-sol" "chatgpt")
                        ("anthropic/claude-fixture" "claude-fixture" "anthropic")
                        ("alpha/team/model" "team/model" "alpha")))
          (destructuring-bind (requested model provider) case
            (check (equal (subseq (multiple-value-list
                                   (parent-provider-resolve-model
                                    registry requested :provider-name provider-name)) 0 2)
                          (list model provider))))))
      (multiple-value-bind (profiles selected-id)
          (parent-provider-configuration-profiles
           '(:model-provider "" :model "gpt-6.1-sol") :registry registry)
        (check (equal selected-id "chatgpt/gpt-6.1-sol"))
        (check (eq ':codex (getf (find selected-id profiles
                                      :key  (lambda (profile) (getf profile :id))
                                      :test #'equal)
                                :family))))
      (multiple-value-bind (profiles selected-id)
          (parent-provider-configuration-profiles '(:model-provider "ALPHA" :model "team/model") :registry registry)
        (check (equal selected-id "alpha/team/model"))
        (check (eq ':custom (getf (find selected-id profiles :key (lambda (spec) (getf spec :id)) :test #'equal)
                                  :family))))
      (multiple-value-bind (profiles selected-id)
          (parent-provider-configuration-profiles '(:model-provider "anthropic" :model "claude-fixture"))
        (check (equal selected-id "anthropic/claude-fixture"))
        (check (eq ':anthropic (getf (first profiles) :family))))
      (let* ((headers  '(("X-Fixture" . "private-header")))
             (options  '(:wire-options (:fixture t)))
             (configuration
               (make-instance 'parent-configuration :values
                              (list :private-root          (namestring temporary)
                                    :account-configuration (list :state-root (namestring temporary))
                                    :profiles              '((:id "builtin-fixture" :family :openrouter
                                                              :model "fixture" :provider-name "openrouter")))))
             (settings (list :provider-endpoint "https://fixture.invalid/setting"
                             :provider-headers headers :provider-options options)))
        (multiple-value-bind (profiles accounts)
            (parent-provider-build-bindings configuration settings :registry registry)
          (let* ((profile (gethash "builtin-fixture" profiles))
                 (account (gethash profile accounts)))
            (check (equal (provider-account-endpoint account) "https://fixture.invalid/setting"))
            (check (equal (provider-account-headers account) headers))
            (check (equal (provider-account-options account) options))))
        (setf (getf (first (getf (parent-configuration-values configuration) :profiles)) :endpoint)
              "https://fixture.invalid/profile")
        (multiple-value-bind (profiles accounts)
            (parent-provider-build-bindings configuration settings :registry registry)
          (check (equal (provider-account-endpoint (gethash (gethash "builtin-fixture" profiles) accounts))
                        "https://fixture.invalid/profile"))))
      (let* ((cache         (merge-pathnames "cache/models.sexp" temporary))
             (configuration '(:secret "fixture-private-token"))
             (first         (make-parent-provider-extension-registry :cache-path cache))
             (second        (make-parent-provider-extension-registry :cache-path cache)))
        (dolist (registry (list first second))
          (let ((*parent-provider-extension-registry* registry))
            (register-parent-provider "cached" :factory         factory
                                      :model-discovery          (lambda (configuration)
                                                                  (declare (ignore configuration)) '("cached-model"))
                                      :model-discovery-endpoint "https://fixture.invalid/cache")))
        (check (null (parent-provider-refresh-models first configuration)))
        (check (probe-file cache))
        (parent-provider-load-model-cache second configuration)
        (check (equal "cached-model" (getf (first (getf (first (parent-provider-projection second)) :models)) :name))))
      (let* ((lock          (bordeaux-threads:make-lock "custom callback test"))
             (state         (list nil nil))
             (configuration (list :lock lock :state state))
             (profile       (make-model-profile :id "blocking" :model "blocking" :family ':responses))
             (account       (make-instance 'parent-extension-account :registry registry :provider-name "blocking"
                                           :settings configuration :family ':custom :protocol ':responses
                                           :credential-manager nil :endpoint "parent-extension"))
             (accounts      (make-hash-table :test #'eq))
             (projection    (make-instance 'cl-llm-provider-api:wire-request :model "blocking"))
             (invocation    (make-instance 'model-invocation :inputs
                                           (list projection #() profile ':generate nil nil nil)
                                           :deadline (+ (session-runtime--clock) 30))))
        (register-parent-provider "blocking" :models '("blocking")
                                  :factory           (lambda (configuration &key reasoning-summaries-p)
                                                       (declare (ignore reasoning-summaries-p))
                                                       (make-instance 'parent-provider-blocking-fixture :configuration configuration)))
        (setf (gethash profile accounts) account)
        (multiple-value-bind (execute abort-invocation)
            (parent-provider-model-executor :accounts accounts :session-id "binding-fixture")
          (let ((thread (bordeaux-threads:make-thread
                         (lambda ()
                           (handler-case (funcall execute invocation (lambda (event) (declare (ignore event))))
                             (error () nil))) :name "trusted callback cancellation check")))
            (unwind-protect
                (progn
                  (loop with deadline = (+ (session-runtime--clock) 5)
                        until (bordeaux-threads:with-lock-held (lock) (first state))
                        do (assert (< (session-runtime--clock) deadline)) (sleep 0.01))
                  (funcall abort-invocation invocation)
                  (check (bordeaux-threads:with-lock-held (lock) (second state)))
                  (bordeaux-threads:join-thread thread)
                  (check (not (bordeaux-threads:thread-alive-p thread)))
                  (funcall abort-invocation invocation))
              (when (bordeaux-threads:thread-alive-p thread)
                (funcall abort-invocation invocation)
                (bordeaux-threads:join-thread thread))))))
      checks)))
