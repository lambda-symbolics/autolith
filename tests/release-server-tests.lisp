(in-package #:autolith)

;;;; -- Release Server Tests --

(-> release-server-tests--write-file (pathname string) pathname)
(defun release-server-tests--write-file (pathname content)
  "Write test CONTENT to PATHNAME and return PATHNAME."
  (ensure-directories-exist pathname)
  (with-open-file (stream pathname
                          :direction :output
                          :if-exists :supersede
                          :if-does-not-exist :create
                          :external-format :utf-8)
    (write-string content stream))
  pathname)

(-> release-server-tests--source-tag (string string) release-source-tag)
(defun release-server-tests--source-tag (name commit)
  "Create one release source-tag fixture with NAME and COMMIT."
  (make-instance 'release-source-tag :name name :commit commit))

(-> release-server-tests--git (pathname list) string)
(defun release-server-tests--git (directory arguments)
  "Run an isolated quiet Git fixture command in DIRECTORY and return output."
  (string-trim
   '(#\Space #\Tab #\Newline #\Return)
   (uiop:run-program
    (append (list "env"
                  "GIT_CONFIG_NOSYSTEM=1"
                  "GIT_CONFIG_GLOBAL=/dev/null"
                  "git" "-C" (namestring directory))
            arguments)
    :output :string
    :error-output :output)))

(-> release-server-tests--git-deployment
    (pathname string &key (:annotated-p boolean) (:updater-p boolean))
    (values pathname release-source-tag))
(defun release-server-tests--git-deployment
    (root version &key annotated-p (updater-p t))
  "Create one exact semantic Git deployment fixture below ROOT."
  (let* ((tag (format nil "v~A" version))
         (deployment (merge-pathnames (format nil "~A/" tag) root)))
    (ensure-directories-exist (merge-pathnames ".keep" deployment))
    (release-server-tests--write-file
     (merge-pathnames "autolith.asd" deployment)
     (format nil
             "(asdf:defsystem #:autolith~%  :version \"~A\"~%)~%"
             version))
    (when updater-p
      (release-server-tests--write-file
       (merge-pathnames "server/release-updater.lisp" deployment)
       "(in-package #:autolith)"))
    (release-server-tests--git deployment '("init" "--quiet"))
    (release-server-tests--git
     deployment '("config" "user.name" "Autolith Release Test"))
    (release-server-tests--git
     deployment '("config" "user.email" "release-test@invalid"))
    (release-server-tests--git deployment '("add" "."))
    (release-server-tests--git
     deployment '("commit" "--quiet" "-m" "Create release fixture"))
    (if annotated-p
        (release-server-tests--git
         deployment (list "tag" "-a" tag "-m" "Annotated fixture"))
        (release-server-tests--git deployment (list "tag" tag)))
    (let ((commit
            (release-server-tests--git deployment '("rev-parse" "HEAD"))))
      (values deployment
              (release-server-tests--source-tag tag commit)))))

(-> test-release-server () null)
(defun test-release-server ()
  "Test semantic release selection and strict HTTP routing."
  (test-assert (release-tag-valid-p "v0.11.1")
               "three-component release tags are valid")
  (test-assert (not (release-tag-valid-p "0.11.1"))
               "release tags require their v prefix")
  (test-assert (not (release-tag-valid-p "v0.11.1.2"))
               "release tags reject extra components")
  (test-assert (release-tag< "v0.9.12" "v0.10.1")
               "release tags compare numeric components")
  (let* ((root
           (uiop:ensure-directory-pathname
            (merge-pathnames
             (format nil "autolith-release-server-tests-~A/" (make-identifier))
             (uiop:temporary-directory))))
         (source-root
           (let ((pathname (merge-pathnames "source/" root)))
             (ensure-directories-exist
              (merge-pathnames ".keep" pathname))
             pathname))
         (public-root (merge-pathnames "public/" root))
         (configuration
           (release-server-configuration-create
            :source-root source-root
            :public-root public-root
            :address "127.0.0.1"
            :port 18098)))
    (unwind-protect
         (progn
           (release-server-tests--write-file
            (merge-pathnames "script/install" source-root)
            (format nil "#!/bin/sh~%"))
           (dolist (tag '("v0.9.12" "v0.10.1"))
             (let* ((directory
                      (release-server--release-directory configuration tag))
                    (archive (release-server--archive-name tag)))
               (release-server-tests--write-file
                (merge-pathnames archive directory)
                "archive")
               (release-server-tests--write-file
                (merge-pathnames (format nil "~A.sha256" archive) directory)
                "checksum")))
           (release-server-tests--write-file
            (merge-pathnames
             (release-server--archive-name "v0.12.0")
             (release-server--release-directory configuration "v0.12.0"))
            "incomplete")
           (test-assert
            (equal (release-server-published-tags configuration)
                   '("v0.9.12" "v0.10.1"))
            "only complete releases enter the semantic publication index")
           (test-assert
            (string= (release-server-latest-tag configuration) "v0.10.1")
            "the newest complete release becomes latest")
           (let ((response (release-server-route configuration "GET" "/autolith")))
             (test-assert (= (release-server-response-status response) 200)
                          "the installer route succeeds")
             (test-assert (pathnamep (release-server-response-body response))
                          "the installer route serves the tracked script"))
           (test-assert
            (string= (release-server-response-body
                      (release-server-route configuration "GET" "/health"))
                     (format nil "ok~%"))
            "health responses contain a real line ending")
           (let* ((identity-configuration
                    (release-server-configuration-create
                     :source-root source-root
                     :public-root public-root
                     :address "127.0.0.1"
                     :port 18098
                     :source-tag "v0.16.1"
                     :source-commit
                     "0123456789abcdef0123456789abcdef01234567"))
                  (matching
                    (release-server-route
                     identity-configuration "GET"
                     "/health/v0.16.1/0123456789abcdef0123456789abcdef01234567"))
                  (stale
                    (release-server-route
                     identity-configuration "GET"
                     "/health/v0.16.0/0123456789abcdef0123456789abcdef01234567")))
             (test-assert
              (= (release-server-response-status matching) 200)
              "source-specific health proves the restarted process identity")
             (test-assert
              (= (release-server-response-status stale) 404)
              "source-specific health rejects a stale server identity"))
           (let ((response
                   (release-server-route configuration "GET" "/releases/latest")))
             (test-assert (= (release-server-response-status response) 302)
                          "the latest route redirects")
             (test-assert
              (equal (assoc "Location"
                            (release-server-response-headers response)
                            :test #'string=)
                     '("Location" . "/releases/v0.10.1"))
              "the latest redirect names the newest complete release"))
           (let* ((tag "v0.10.1")
                  (archive (release-server--archive-name tag))
                  (response
                    (release-server-route
                     configuration "HEAD"
                     (format nil "/releases/~A/~A" tag archive))))
             (test-assert (= (release-server-response-status response) 200)
                          "published archives support HEAD")
             (test-assert
              (string= (release-server-response-content-type response)
                       "application/gzip")
              "archive responses use the gzip media type"))
           (let* ((tag "v0.10.1")
                  (checksum
                    (format nil "~A.sha256" (release-server--archive-name tag)))
                  (response
                    (release-server-route
                     configuration "GET"
                     (format nil "/releases/~A/~A" tag checksum))))
             (test-assert
              (string= (release-server-response-content-type response)
                       "text/plain; charset=utf-8")
              "checksum responses use the plain-text media type"))
           (test-assert
            (= (release-server-response-status
                (release-server-route
                 configuration "GET" "/releases/v0.10.1/../secret"))
               404)
            "release routes reject filesystem traversal")
           (test-assert
            (= (release-server-response-status
                (release-server-route configuration "POST" "/autolith"))
               405)
            "release routes reject mutating HTTP methods")
           (multiple-value-bind (method target)
               (release-server--parse-request-head
                (format nil "GET /health HTTP/1.1~C~CHost: localhost~C~C~C~C"
                        #\Return #\Newline #\Return #\Newline
                        #\Return #\Newline))
             (test-assert (and (string= method "GET")
                               (string= target "/health"))
                          "HTTP request parsing returns its method and target"))
           (test-assert
            (handler-case
                (progn
                  (release-server--parse-request-head
                   (format nil "broken~C~C~C~C"
                           #\Return #\Newline #\Return #\Newline))
                  nil)
              (release-server-request-error ()
                t))
            "malformed HTTP request lines signal a structured condition"))
      (uiop:delete-directory-tree root :validate t :if-does-not-exist :ignore)))
  (let* ((root
           (uiop:ensure-directory-pathname
            (merge-pathnames
             (format nil "autolith-release-identity-tests-~A/"
                     (make-identifier))
             (uiop:temporary-directory))))
         (source-a (merge-pathnames "source-a/" root))
         (source-b (merge-pathnames "source-b/" root)))
    (unwind-protect
         (progn
           (release-server-tests--write-file
            (merge-pathnames "fixture.txt" source-a)
            "deterministic release identity")
           (release-server-tests--write-file
            (merge-pathnames "fixture.txt" source-b)
            "deterministic release identity")
           (sb-posix:utime
            (namestring (merge-pathnames "fixture.txt" source-a))
            1 1)
           (sb-posix:utime
            (namestring (merge-pathnames "fixture.txt" source-b))
            2 2)
           (release-archive--create-source-identity
            source-a "v0.16.0" "1700000000")
           (release-archive--create-source-identity
            source-b "v0.16.0" "1700000000")
           (multiple-value-bind (output error-output status)
               (uiop:run-program
                (list "diff" "-r"
                      (namestring (merge-pathnames ".git/" source-a))
                      (namestring (merge-pathnames ".git/" source-b)))
                :output ':string
                :error-output ':string
                :ignore-error-status t)
             (test-assert
              (eql status 0)
              (format nil
                      "packaged source identities ignore source stat metadata:~%~A~A"
                      output error-output)))
           (test-assert
            (and (not (probe-file (merge-pathnames ".git/hooks/" source-a)))
                 (not (probe-file (merge-pathnames ".git/logs/" source-a))))
            "packaged source identities omit template and reflog state"))
      (uiop:delete-directory-tree root
                                  :validate t
                                  :if-does-not-exist :ignore)))
  (let* ((root
           (uiop:ensure-directory-pathname
            (merge-pathnames
             (format nil "autolith-release-archive-tests-~A/" (make-identifier))
             (uiop:temporary-directory))))
         (dependency-root (merge-pathnames "dependencies/" root))
         (target-root (merge-pathnames "target/" root))
         (valid-link (merge-pathnames "valid" dependency-root))
         (broken-link (merge-pathnames "broken" dependency-root)))
    (unwind-protect
         (progn
           (release-server-tests--write-file
            (merge-pathnames "system.asd" target-root)
            "(asdf:defsystem #:fixture)")
           (ensure-directories-exist (merge-pathnames ".keep" dependency-root))
           (sb-posix:symlink (namestring target-root) (namestring valid-link))
           (sb-posix:symlink (namestring (merge-pathnames "missing/" root))
                             (namestring broken-link))
           (release-archive--materialize-dependency-links dependency-root)
           (test-assert
            (probe-file (merge-pathnames "valid/system.asd" dependency-root))
            "release archives replace dependency links with private copies")
           (test-assert
            (not (probe-file broken-link))
            "release archives remove broken dependency links"))
      (uiop:delete-directory-tree root :validate t :if-does-not-exist :ignore)))
  (let* ((commit-a "0123456789abcdef0123456789abcdef01234567")
           (commit-b "89abcdef0123456789abcdef0123456789abcdef")
           (tag-object "fedcba9876543210fedcba9876543210fedcba98")
           (peeled "abcdef0123456789abcdef0123456789abcdef01")
           (parsed
             (release-builder--parse-remote-tags
              (format nil "~A~Crefs/tags/v0.11.2~%~A~Crefs/tags/v0.10.9~%"
                      commit-a #\Tab commit-b #\Tab)))
           (annotated
             (release-builder--parse-remote-tags
              (format nil
                      "~A~Crefs/tags/v0.12.0~%~A~Crefs/tags/v0.12.0^{}~%"
                      tag-object #\Tab peeled #\Tab))))
      (test-assert
       (equal (mapcar #'release-source-tag-name parsed)
              '("v0.10.9" "v0.11.2"))
       "remote release tags are validated and sorted semantically")
      (test-assert
       (and
        (equal (mapcar #'release-source-tag-name annotated)
               '("v0.12.0"))
        (equal (release-source-tag-commit (first annotated))
               peeled))
       "annotated remote tags prefer the peeled commit identity"))
  (let* ((root
           (uiop:ensure-directory-pathname
            (merge-pathnames
             (format nil "autolith-release-builder-tests-~A/" (make-identifier))
             (uiop:temporary-directory))))
         (source-root (merge-pathnames "source/" root))
         (public-root (merge-pathnames "public/" root))
         (state-root (merge-pathnames "state/" root))
         (builder
           (release-builder-configuration-create
            :source-root source-root
            :state-root state-root
            :public-root public-root
            :repository "https://example.invalid/autolith.git"
            :poll-seconds 30
            :container-command "container-test"))
         (commit "0123456789abcdef0123456789abcdef01234567")
         (tags (list (release-server-tests--source-tag "v0.11.0" commit)
                     (release-server-tests--source-tag "v0.11.1" commit)
                     (release-server-tests--source-tag "v0.12.0" commit))))
    (unwind-protect
         (progn
           (test-assert
            (equal (mapcar #'release-source-tag-name
                           (release-builder-pending-tags builder tags))
                   '("v0.12.0"))
            "an empty builder starts from only the newest remote tag")
           (let* ((server
                    (release-server-configuration-create
                     :source-root source-root
                     :public-root public-root))
                  (tag "v0.11.0")
                  (directory
                    (release-server--release-directory server tag))
                  (archive (release-server--archive-name tag)))
             (release-server-tests--write-file
              (merge-pathnames archive directory) "archive")
             (release-server-tests--write-file
              (merge-pathnames (format nil "~A.sha256" archive) directory)
              "checksum"))
           (test-assert
            (equal (mapcar #'release-source-tag-name
                           (release-builder-pending-tags builder tags))
                   '("v0.11.1" "v0.12.0"))
            "a builder catches up every tag newer than its latest publication")
           (release-server-tests--write-file
            (merge-pathnames "autolith.asd" source-root)
            (format nil
                    "(asdf:defsystem #:autolith~%  :version \"0.11.2\"~%)~%"))
           (test-assert
            (string= (release-builder--source-version source-root) "0.11.2")
            "builder source validation reads the declared ASDF version"))
      (uiop:delete-directory-tree root :validate t :if-does-not-exist :ignore)))
  (let* ((root
           (uiop:ensure-directory-pathname
            (merge-pathnames
             (format nil "autolith-release-updater-tests-~A/"
                     (make-identifier))
             (uiop:temporary-directory))))
         (deployments-root (merge-pathnames "deployments/" root))
         (state-root (merge-pathnames "updater-state/" root))
         (host-lock-root (merge-pathnames "builder-state/" root))
         (selection-path (merge-pathnames "current" root))
         (old-path nil)
         (old-tag nil)
         (next-path nil)
         (next-tag nil))
    (unwind-protect
         (progn
           (multiple-value-setq (old-path old-tag)
             (release-server-tests--git-deployment
              deployments-root "0.16.0" :updater-p nil))
           (multiple-value-setq (next-path next-tag)
             (release-server-tests--git-deployment
              deployments-root "0.16.1"))
           (release-updater--replace-symlink selection-path old-path)
           (let ((identity-configuration
                   (release-server-configuration-create
                    :source-root selection-path
                    :public-root (merge-pathnames "public/" root))))
             (test-assert
              (equal
               (release-server-configuration-source-root
                identity-configuration)
               (truename old-path))
              "release server captures the canonical selected deployment")
             (test-assert
              (and
               (string=
                (release-server-configuration-source-tag
                 identity-configuration)
                (release-source-tag-name old-tag))
               (string=
                (release-server-configuration-source-commit
                 identity-configuration)
                (release-source-tag-commit old-tag)))
              "safe Git identity captures a root-owned lightweight deployment"))
           (let ((configuration
                   (release-updater-configuration-create
                    :selection-path selection-path
                    :deployments-root deployments-root
                    :state-root state-root
                    :host-lock-root host-lock-root
                    :repository "https://example.invalid/autolith.git"
                    :poll-seconds 1
                    :activation-timeout-seconds 1
                    :server-service (merge-pathnames "server-service/" root)
                    :builder-service (merge-pathnames "builder-service/" root)
                    :health-url "http://127.0.0.1:18098/health"
                    :service-account "nobody"
                    :service-home (merge-pathnames "service-home/" root)))
                 (restart-count 0))
             (let* ((commands nil)
                    (*release-updater-service-command-function*
                      (lambda (command &key output error-output)
                        (declare (ignore error-output))
                        (push command commands)
                        (if (eq output ':string)
                            "true 4242"
                            nil))))
               (release-updater--service-cycle
                configuration
                (release-updater-configuration-server-service configuration))
               (test-assert
                (and (= (length commands) 2)
                     (member "-wd" (second commands) :test #'string=)
                     (member "-d" (second commands) :test #'string=)
                     (member "-wu" (first commands) :test #'string=)
                     (member "-u" (first commands) :test #'string=))
                "activation explicitly waits for service down before waited up")
               (test-assert
                (release-updater--service-up-p
                 (release-updater-configuration-builder-service configuration))
                "builder activation verifies a live positive process identity"))
             (test-assert
              (release-updater--state-valid-p
               (release-updater--state ':active old-tag))
              "native updater states validate without a phantom list element")
             (release-updater--write-state
              configuration
              (release-updater--state ':active old-tag))
             (test-assert
              (equal (release-updater--read-state configuration)
                     (release-updater--state ':active old-tag))
              "native updater state round trips as one strict readable form")
             (release-updater-promote
              configuration next-path next-tag
              :restart-function
              (lambda (ignored)
                (declare (ignore ignored))
                (incf restart-count)
                (test-assert
                 (equal (truename selection-path) (truename next-path))
                 "activation observes the atomically selected final deployment")))
             (test-assert (= restart-count 1)
                          "successful promotion activates services once")
             (test-assert
              (equal (truename selection-path) (truename next-path))
              "successful promotion retains the new current selection")
             (test-assert
              (eq (getf (rest (release-updater--read-state configuration))
                        ':phase)
                  ':active)
              "successful promotion durably reaches the active phase")
             (multiple-value-bind (failed-path failed-tag)
                 (release-server-tests--git-deployment
                  deployments-root "0.16.2")
               (let ((activation-count 0)
                     (failed-p nil))
                 (handler-case
                     (release-updater-promote
                      configuration failed-path failed-tag
                      :restart-function
                      (lambda (ignored)
                        (declare (ignore ignored))
                        (incf activation-count)
                        (when (= activation-count 1)
                          (error "Injected activation failure."))))
                   (release-updater-error ()
                     (setf failed-p t)))
                 (test-assert failed-p
                              "activation failure remains structured")
                 (test-assert (= activation-count 2)
                              "failed activation restarts the rollback selection")
                 (test-assert
                  (equal (truename selection-path) (truename next-path))
                  "failed activation atomically restores last known good")
                 (let ((state (release-updater--read-state configuration)))
                   (test-assert
                    (string=
                     (getf (rest state) ':failed-tag)
                     (release-source-tag-name failed-tag))
                    "failed immutable activation is durably quarantined")
                   (multiple-value-bind (newer-path newer-tag)
                       (release-server-tests--git-deployment
                        deployments-root "0.16.3")
                     (declare (ignore newer-path))
                     (test-assert
                      (eq
                       (release-updater--pending-tag
                        next-tag (list failed-tag newer-tag) state)
                       newer-tag)
                      "a failed tag does not block a later semantic release")
                     (test-assert
                      (null
                       (release-updater--pending-tag
                        next-tag (list failed-tag) state))
                      "a deterministic failed tag is not retried forever")))))
             (multiple-value-bind (recovery-path recovery-tag)
                 (release-server-tests--git-deployment
                  deployments-root "0.16.4")
               (release-updater--replace-symlink selection-path next-path)
               (release-updater--write-state
                configuration
                (release-updater--state
                 ':prepared recovery-tag :previous next-tag))
               (let ((recovery-restarts 0))
                 (release-updater--reconcile
                  configuration
                  (lambda (ignored)
                    (declare (ignore ignored))
                    (incf recovery-restarts)))
                 (test-assert (= recovery-restarts 1)
                              "an interrupted prepared transaction resumes once")
                 (test-assert
                  (equal (truename selection-path) (truename recovery-path))
                  "reconciliation completes the prepared atomic selection"))))
           (multiple-value-bind (annotated-path annotated-tag)
               (release-server-tests--git-deployment
                (merge-pathnames "annotated/" root)
                "0.17.0"
                :annotated-p t)
             (test-assert
              (not
               (release-updater--checkout-valid-p
                annotated-path annotated-tag))
              "host deployments reject annotated release tags")))
      (uiop:delete-directory-tree root
                                  :validate t
                                  :if-does-not-exist :ignore)))
  (let* ((root
           (uiop:ensure-directory-pathname
            (merge-pathnames
             (format nil "autolith-release-final-path-tests-~A/"
                     (make-identifier))
             (uiop:temporary-directory))))
         (remote-root (merge-pathnames "remote/" root))
         (deployments-root (merge-pathnames "deployments/" root))
         (configuration
           (release-updater-configuration-create
            :selection-path (merge-pathnames "current" root)
            :deployments-root deployments-root
            :state-root (merge-pathnames "updater-state/" root)
            :host-lock-root (merge-pathnames "builder-state/" root)
            :repository
            (namestring (merge-pathnames "v0.18.0/" remote-root))
            :poll-seconds 1
            :activation-timeout-seconds 1
            :server-service (merge-pathnames "server-service/" root)
            :builder-service (merge-pathnames "builder-service/" root)
            :health-url "http://127.0.0.1:18098/health"
            :service-account "nobody"
            :service-home (merge-pathnames "service-home/" root))))
    (unwind-protect
         (multiple-value-bind (remote-path source-tag)
             (release-server-tests--git-deployment remote-root "0.18.0")
           (declare (ignore remote-path))
           (let ((setup-called-p nil)
                 (final
                   (release-updater--deployment-path
                    configuration source-tag)))
             (let* ((commands nil)
                    (*release-updater-host-command-function*
                      (lambda (command
                               &key directory input output error-output)
                        (declare
                         (ignore directory input output error-output))
                        (push command commands)
                        nil)))
               (release-updater--grant-service-write-access
                configuration final)
               (test-assert
                (and
                 (= (length commands) 2)
                 (some
                  (lambda (command)
                    (member
                     (namestring
                      (release-updater-configuration-service-home
                       configuration))
                     command
                     :test #'string=))
                  commands)
                 (some
                  (lambda (command)
                    (member (namestring final) command :test #'string=))
                  commands))
                "candidate setup makes both private runtime and source writable"))
             (release-updater-prepare-deployment
              configuration source-tag
              :setup-function
              (lambda (ignored candidate ignored-tag)
                (declare (ignore ignored ignored-tag))
                (setf setup-called-p t)
                (test-assert
                 (equal (truename candidate) (truename final))
                 "bootstrap and checks run only after the final-path rename")
                (test-assert
                 (not
                  (uiop:directory-exists-p
                   (merge-pathnames
                    (format nil ".~A.incoming.~D/"
                            (release-source-tag-name source-tag)
                            (sb-posix:getpid))
                    deployments-root)))
                 "no incoming pathname survives into candidate setup")))
             (test-assert setup-called-p
                          "final-path candidate setup is invoked")
             (release-server-tests--write-file
              (merge-pathnames "autolith.asd"
                               (merge-pathnames "v0.18.0/" remote-root))
              (format nil
                      "(asdf:defsystem #:autolith~%  :version \"0.18.1\"~%)~%"))
             (release-server-tests--git
              (merge-pathnames "v0.18.0/" remote-root)
              '("add" "autolith.asd"))
             (release-server-tests--git
              (merge-pathnames "v0.18.0/" remote-root)
              '("commit" "--quiet" "-m" "Advance release fixture"))
             (release-server-tests--git
              (merge-pathnames "v0.18.0/" remote-root)
              '("tag" "v0.18.1"))
             (let* ((commit
                      (release-server-tests--git
                       (merge-pathnames "v0.18.0/" remote-root)
                       '("rev-parse" "HEAD")))
                    (failed-tag
                      (release-server-tests--source-tag "v0.18.1" commit))
                    (failed-path
                      (release-updater--deployment-path
                       configuration failed-tag))
                    (failed-p nil))
               (handler-case
                   (release-updater-prepare-deployment
                    configuration failed-tag
                    :setup-function
                    (lambda (ignored candidate ignored-tag)
                      (declare (ignore ignored candidate ignored-tag))
                      (error "Injected final-path setup failure.")))
                 (release-updater-error ()
                   (setf failed-p t)))
               (test-assert failed-p
                            "final-path setup failures remain structured")
               (test-assert
                (not (uiop:directory-exists-p failed-path))
                "a failed updater-owned final candidate is removed for retry")))
      (uiop:delete-directory-tree root
                                  :validate t
                                  :if-does-not-exist :ignore)))
  nil))
