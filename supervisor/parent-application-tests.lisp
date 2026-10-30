;;;; -- Parent Application Composition Checks --

(in-package #:autolith-supervisor)

(serapeum/types:-> parent-application-test--data (pathname) list)
(defun parent-application-test--data (root)
  "Return a trusted test configuration without credentials."
  (flet ((path (name) (namestring (merge-pathnames name root))))
    (list :version 1 :runtime "/usr/bin/false" :agent-core (path "agent.core")
          :worker-core (path "agent.core") :source-root (path "source/")
          :workspace (path "workspace/") :private-root (path "private/")
          :temporary-root (path "temporary/") :runtime-roots (list (path "source/"))
          :account-configuration (list :state-root (path "private/accounts/"))
          :profiles              '((:id "ordinary" :family :openai-api :model "test-model"))
          :profile-id "ordinary" :access-p nil)))

(serapeum/types:-> parent-application-test--rejects (function) boolean)
(defun parent-application-test--rejects (function)
  "Return whether a configuration operation rejects its input."
  (handler-case (progn (funcall function) nil)
    (parent-configuration-error () t)))

(serapeum/types:-> parent-application-test--cleanup (pathname) null)
(defun parent-application-test--cleanup (directory)
  "Remove only ordinary files and directories created by this fixture."
  (when (uiop:directory-exists-p directory)
    (dolist (file (uiop:directory-files directory)) (delete-file file))
    (dolist (child (uiop:subdirectories directory)) (parent-application-test--cleanup child))
    (uiop:delete-empty-directory directory))
  nil)

(serapeum/types:-> run-parent-application-tests () integer)
(defun run-parent-application-tests ()
  "Check the real configuration, provider bindings, record composition and durable history."
  (assert (not (find-package '#:autolith)))
  (let* ((root               (merge-pathnames (format nil "parent-application-~A/"
                                                     (parent-application--identifier))
                                              (uiop:temporary-directory)))
         (data               (parent-application-test--data root))
         (configuration      (make-parent-configuration data))
         (configuration-path (merge-pathnames "configuration.sexp" root)))
    (unwind-protect
         (progn
           (ensure-directories-exist configuration-path)
           (dolist (key '(:workspace :temporary-root :private-root :source-root))
             (ensure-directories-exist
              (merge-pathnames "probe" (parent-configuration-get configuration key))))
           (dolist (bad (list (append data '(:unknown t))
                             (append data '(:profile-id "other"))
                             (append data '(:knowledge-mode "invalid"))
                             (append data '(:allow-global-p "yes"))
                             (let ((copy (copy-list data))) (setf (getf copy :runtime) "relative") copy)
                             (let ((copy (copy-list data))) (setf (getf copy :profile-id) "other") copy)
                             (let ((copy (copy-list data)))
                               (setf (getf copy :profiles)
                                     '((:id "same" :family :openai-api :model "model")
                                       (:id "same" :family :openai-api :model "model"))) copy)
                             (let ((copy (copy-list data)))
                               (setf (getf copy :profiles) (list (list :id "bad" :family ':openai-api
                                                                    :model "model" :options #'identity))) copy)))
             (assert (parent-application-test--rejects (lambda () (make-parent-configuration bad)))))
           (with-open-file (stream configuration-path :direction ':output :if-exists ':supersede)
             (write data :stream stream :readably t))
           (assert (typep (read-parent-configuration configuration-path) 'parent-configuration))
           (with-open-file (stream configuration-path :direction ':output :if-exists ':supersede)
             (write-string "#.(error \"must not execute\")" stream))
           (assert (handler-case (progn (read-parent-configuration configuration-path) nil)
                     (reader-error () t)))
           (with-open-file (stream configuration-path :direction ':output :if-exists ':supersede)
             (write data :stream stream :readably t) (write data :stream stream :readably t))
           (assert (parent-application-test--rejects
                    (lambda () (read-parent-configuration configuration-path))))
           (let ((environment (parent-configuration-child-environment configuration))
                 (public      (protocol-map->data (parent-configuration-agent-map configuration))))
             (assert (hash-table-p (gethash "settings" public)))
             (assert (equal (gethash "knowledge_mode" public) "active"))
             (assert (equal (gethash "profile_id" public) "ordinary"))
             (assert (not (find-if (lambda (entry) (or (search "TOKEN" entry) (search "API_KEY" entry)
                                                        (search "private/" entry))) environment)))
             (assert (not (gethash "account-configuration" public)))
             (assert (not (gethash "endpoint" public))))
           (let* ((source      (lambda (timeout) (declare (ignore timeout)) (values nil ':eof)))
                  (renderer    (lambda (region kind payload) (declare (ignore region kind payload))))
                  (application (make-parent-application configuration :session-id "resume-session"
                                                         :input-source source :renderer renderer))
                  (store       (session-store-open (parent-application--path application "session.sexp")
                                                   "resume-session")))
              (assert (typep
                       (make-access-context
                        :pathname      (parent-application--path application "policy.sexp")
                        :session-id    "resume-session"
                        :workspace     (parent-configuration-get configuration ':workspace)
                        :private-roots (parent-application-private-roots configuration)
                        :backend       (make-instance 'access-backend)
                        :terminal      (parent-application--terminal application))
                       'access-context))
             (assert (= (hash-table-count (parent-application-profiles application)) 1))
             (assert (= (hash-table-count (parent-application-accounts application)) 1))
             (let ((knowledge (parent-application-knowledge application)))
               (assert (not (knowledge-service-allow-global-p knowledge)))
               (assert (equal (knowledge-service-session-id knowledge) "resume-session"))
               (assert (not (funcall (knowledge-service-approve knowledge)
                                    ':rule ':workspace "proposal" 1 nil))))
             (dolist (mode '("active" "passive" "off"))
               (let* ((copy     (copy-tree data))
                      (approved nil)
                      (approve  (lambda (&rest arguments) (setf approved arguments) t)))
                 (setf (getf copy :allow-global-p) t
                       (getf copy :knowledge-mode) mode)
                 (let* ((configured (make-parent-configuration copy))
                        (owner      (make-parent-application configured :session-id "global-enabled"
                                                             :input-source source :renderer renderer
                                                             :knowledge-approval    approve))
                        (knowledge  (parent-application-knowledge owner)))
                   (assert (equal (parent-configuration-get configured ':knowledge-mode) mode))
                   (assert (knowledge-service-allow-global-p knowledge))
                   (assert (funcall (knowledge-service-approve knowledge)
                                    ':contract ':global "owner-proposal" 2 '(:exact t)))
                   (assert (equal approved '(:contract :global "owner-proposal" 2 (:exact t)))))))
             (dolist (key '(:api-keys-path :grok-bootstrap-auth-path))
               (dolist (name '("existing-credential.sexp" "future-credential.sexp"))
                 (let* ((copy (copy-tree data))
                        (path (merge-pathnames name (parent-configuration-get configuration ':workspace))))
                   (when (equal name "existing-credential.sexp")
                     (with-open-file (stream path :direction ':output :if-exists ':supersede)
                       (write-string "fixture data" stream)))
                   (setf (getf (getf copy :account-configuration) key) (namestring path))
                   (assert (parent-application-test--rejects
                            (lambda () (parent-application-private-roots (make-parent-configuration copy))))))))
             (let* ((copy       (copy-tree data))
                    (credential (merge-pathnames "external-account/key.sexp" root)))
               (ensure-directories-exist credential)
               (with-open-file (stream credential :direction ':output) (write-string "fixture data" stream))
               (setf (getf (getf copy :account-configuration) :api-keys-path) (namestring credential))
               (let ((roots (parent-application-private-roots (make-parent-configuration copy))))
                 (assert (member (namestring (ls-compat.posix:resolve-pathname credential)) roots :test #'equal))
                 (assert (member (namestring (ls-compat.posix:resolve-pathname
                                              (uiop:pathname-directory-pathname credential))) roots :test #'equal))))
             (session-store-start-turn store "trusted-turn" "parent-owned user words")
             (let ((note    (make-hash-table :test #'equal))
                   (outcome (make-hash-table :test #'equal)))
               (setf (gethash "text" note) "not user input"
                     (gethash "access_outcome" note) outcome
                     (gethash "kind" outcome) "denied"
                     (gethash "operation_id" outcome) "operation"
                     (gethash "sequence" outcome) 1
                     (gethash "policy_revision" outcome) "0")
               (session-store-parent-note store :event-id "parent-note" :payload note))
             (assert (equal (parent-application-input-history application) '("parent-owned user words")))
             (let* ((records  (parent-application-records application))
                    (bindings (record-dispatcher-bindings records))
                    (memory   (rest (assoc ':memory bindings))))
               (assert (typep (rest (assoc ':agenda bindings)) 'agenda-service))
               (assert (typep (rest (assoc ':papercut bindings)) 'papercut-service))
               (assert (equal (funcall (memory-service-input-history memory)) '("parent-owned user words"))))
             (let ((resumed (make-parent-application configuration :session-id "resume-session"
                                                     :input-source source :renderer renderer)))
               (assert (equal (parent-application-directory resumed) (parent-application-directory application)))
               (assert (equal (parent-application-input-history resumed) '("parent-owned user words"))))
             (dolist (identifier '("../escape" "a/b" "" "."))
               (assert (parent-application-test--rejects
                        (lambda () (make-parent-application configuration :session-id identifier
                                                             :input-source source :renderer renderer))))))
           (assert (not (find-package '#:autolith)))
           41)
      (parent-application-test--cleanup root))))
