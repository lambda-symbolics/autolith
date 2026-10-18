(in-package #:autolith)

;;;; -- Plugin Tests --

(-> plugin-tests--identity (string &key (:source string)) plugin-identity)
(defun plugin-tests--identity (commit &key source)
  "Return one resolved plugin identity for the tests."
  (make-instance 'plugin-identity :source source :commit commit))

(-> plugin-tests--lock-form () list)
(defun plugin-tests--lock-form ()
  "Return one complete lock form with a dependency, a winner, and a conflict."
  '(:plugins-lock
    :version 1
    :plugins
    ((:plugin
      :name "utils"
      :source "github.com/example/utils"
      :ref (:tag "v1.1.0")
      :commit "a91c04f0"
      :checkout "plugins/checkouts/github-com-example-utils-a91c04f0/"
      :dependencies
      ((:dependency
        :name "logging"
        :source "github.com/example/logging"
        :commit "bbb222"))
      :winner
      (:requirement
       :scope :user
       :requester nil
       :source "github.com/example/utils"
       :ref (:tag "v1.1.0")
       :commit "a91c04f0")
      :conflicts
      ((:conflict
        :loser
        (:requirement
         :scope :plugin
         :requester "demo"
         :source "github.com/example/utils"
         :ref (:tag "v1.0.0")
         :commit "ccc333")
        :winner
        (:requirement
         :scope :user
         :requester nil
         :source "github.com/example/utils"
         :ref (:tag "v1.1.0")
         :commit "a91c04f0")))))))

(-> test-plugin-identity () null)
(defun test-plugin-identity ()
  "Test plugin identity equality, portable form, and printed form."
    (let* ((identity (plugin-tests--identity "a91c04f0"
                                             :source "github.com/example/utils"))
           (same (plugin-tests--identity "a91c04f0"
                                         :source "github.com/example/utils"))
           (other-commit (plugin-tests--identity "0badc0de"
                                                 :source "github.com/example/utils"))
         (other-source (plugin-tests--identity "a91c04f0"
                                               :source "github.com/example/fork")))
    (test-assert (plugin-identity-p identity)
                 "a plugin identity is a plugin identity")
    (test-assert (plugin-identity-equal-p identity same)
                 "equal source and commit define identity equality")
    (test-assert (not (plugin-identity-equal-p identity other-commit))
                 "a different commit is a different identity")
    (test-assert (not (plugin-identity-equal-p identity other-source))
                 "a different source is a different identity")
    (test-assert (not (plugin-identity-equal-p identity nil))
                 "a non-identity is never equal")
    (test-assert (equal (plugin-identity->form identity)
                        '(:plugin-identity
                          :source "github.com/example/utils"
                          :commit "a91c04f0"))
                 "the identity form is readable sexp data")
    (let ((printed (prin1-to-string identity)))
      (test-assert (and (search "github.com/example/utils" printed)
                        (search "a91c04f0" printed))
                   "the printed identity names its source and commit"))))

(-> test-plugin-record () null)
(defun test-plugin-record ()
  "Test the phase 1 plugin record and its state flow."
  (let ((record (make-instance 'plugin-record
                               :name "utils"
                               :source "github.com/example/utils"
                               :ref '(:tag "v1.1.0"))))
    (test-assert (eq (plugin-record-state record) ':declared)
                 "a new plugin record is declared")
      (plugin-record-resolve record (plugin-tests--identity "a91c04f0"
                                                            :source "github.com/example/utils"))
    (test-assert (plugin-identity-equal-p (plugin-record-identity record)
                                            (plugin-tests--identity "a91c04f0"
                                                                    :source "github.com/example/utils"))
                 "resolution attaches the plugin identity")
    (plugin-record-transition record ':enabled)
    (test-assert (eq (plugin-record-state record) ':enabled)
                 "a declared plugin can become enabled")
    (test-assert (handler-case
                     (progn (plugin-record-transition record ':failed
                                                      :reason "late")
                            nil)
                   (plugin-record-error () t))
                 "an enabled plugin cannot become failed"))
  (let ((record (make-instance 'plugin-record
                               :name "demo"
                               :source "github.com/example/demo")))
    (plugin-record-transition record ':failed :reason "missing system")
    (test-assert (and (eq (plugin-record-state record) ':failed)
                      (string= (plugin-record-failure record) "missing system"))
                 "a failed plugin keeps its reason")
    (plugin-record-transition record ':disabled)
    (test-assert (and (eq (plugin-record-state record) ':disabled)
                      (null (plugin-record-failure record)))
                 "a failed plugin can become disabled"))
  (let ((record (make-instance 'plugin-record
                               :name "disabled"
                               :source "github.com/example/disabled"
                               :state ':disabled)))
    (plugin-record-transition record ':declared)
    (test-assert (eq (plugin-record-state record) ':declared)
                 "a disabled plugin can be declared again"))
  (test-assert (handler-case
                   (progn (plugin-record-transition
                           (make-instance 'plugin-record
                                          :name "bad"
                                          :source "github.com/example/bad")
                           ':failed)
                          nil)
                 (plugin-record-error () t))
               "a failed transition needs a reason"))

(-> test-plugin-store-paths () null)
(defun test-plugin-store-paths ()
  "Test the plugin store layout without creating directories."
  (with-test-configuration (configuration)
      (let* ((identity (plugin-tests--identity "a91c04f0"
                                               :source "github.com/example/utils"))
           (root (plugin-store-root configuration))
           (checkout-root (plugin-store-checkout-root configuration))
           (checkout (plugin-store-checkout-directory configuration identity)))
      (test-assert (equal (namestring root)
                          (namestring (merge-pathnames
                                       "plugins/"
                                       (config :state-root configuration))))
                   "the plugin store is below the state root")
      (test-assert (equal (namestring checkout-root)
                          (namestring (merge-pathnames "checkouts/" root)))
                   "checkouts have a dedicated subdirectory")
      (test-assert (equal (plugin-store-checkout-name identity)
                          "github-com-example-utils-a91c04f0")
                   "the checkout name is a readable source and commit slug")
      (test-assert (equal (namestring checkout)
                          (namestring (merge-pathnames
                                       "github-com-example-utils-a91c04f0/"
                                       checkout-root)))
                   "the checkout path is computed from the identity")
      (test-assert (not (uiop:directory-exists-p checkout))
                   "computing a checkout path does not create it"))))

(-> test-plugin-lock-format () null)
(defun test-plugin-lock-format ()
  "Test the literal plugin lock form and malformed shapes."
  (let ((form (plugin-tests--lock-form)))
    (test-assert (plugin-lock-form-p form)
                 "the complete lock form is supported")
    (test-assert (equal form (plugin-lock->form (form->plugin-lock form)))
                 "the lock form round-trips through the data model")
    (let* ((lock (form->plugin-lock form))
           (entry (plugin-lock-entry-find lock "utils")))
      (test-assert (and entry
                        (string= (plugin-lock-entry-name entry) "utils")
                        (equal (plugin-lock-entry-checkout entry)
                               "plugins/checkouts/github-com-example-utils-a91c04f0/"))
                   "an entry is found by plugin name")
      (test-assert (and (= (length (plugin-lock-entry-dependencies entry)) 1)
                        (string= (plugin-lock-dependency-name
                                  (first (plugin-lock-entry-dependencies entry)))
                                 "logging"))
                   "the entry records its transitive dependency edge")
      (test-assert (and (plugin-lock-entry-winner entry)
                        (eq (plugin-requirement-scope
                             (plugin-lock-entry-winner entry))
                            ':user))
                   "the entry records the override winner")
      (let ((conflict (first (plugin-lock-entry-conflicts entry))))
        (test-assert (and conflict
                          (string= (plugin-requirement-requester
                                    (plugin-lock-conflict-loser conflict))
                                   "demo")
                          (string= (plugin-requirement-commit
                                    (plugin-lock-conflict-winner conflict))
                                   "a91c04f0"))
                     "the conflict keeps the loser beside the winner")))
    (dolist (case
             (list
              (list "a wrong top-level tag is rejected"
                    '(:plugin-lock :version 1 :plugins nil))
              (list "an unsupported version is rejected"
                    '(:plugins-lock :version 2 :plugins nil))
              (list "a missing plugin list is rejected"
                    '(:plugins-lock :version 1))
              (list "a non-list plugin list is rejected"
                    '(:plugins-lock :version 1 :plugins 3))
              (list "an entry without a commit is rejected"
                    '(:plugins-lock
                      :version 1
                      :plugins
                      ((:plugin :name "utils"
                                :source "github.com/example/utils"
                                :ref nil
                                :checkout "plugins/checkouts/utils/"
                                :dependencies nil
                                :winner nil
                                :conflicts nil))))
              (list "an odd ref is rejected"
                    '(:plugins-lock
                      :version 1
                      :plugins
                      ((:plugin :name "utils"
                                :source "github.com/example/utils"
                                :ref (:tag)
                                :commit "a91c04f0"
                                :checkout "plugins/checkouts/utils/"
                                :dependencies nil
                                :winner nil
                                :conflicts nil))))
              (list "duplicate plugin names are rejected"
                    '(:plugins-lock
                      :version 1
                      :plugins
                      ((:plugin :name "utils"
                                :source "github.com/example/utils"
                                :ref nil
                                :commit "a91c04f0"
                                :checkout "plugins/checkouts/utils/"
                                :dependencies nil
                                :winner nil
                                :conflicts nil)
                       (:plugin :name "utils"
                                :source "github.com/example/other"
                                :ref nil
                                :commit "bbb222"
                                :checkout "plugins/checkouts/other/"
                                :dependencies nil
                                :winner nil
                                :conflicts nil))))
              (list "a non-plugin requirement with a requester is rejected"
                    '(:plugins-lock
                      :version 1
                      :plugins
                      ((:plugin :name "utils"
                                :source "github.com/example/utils"
                                :ref nil
                                :commit "a91c04f0"
                                :checkout "plugins/checkouts/utils/"
                                :dependencies nil
                                :winner (:requirement
                                         :scope :user
                                         :requester "demo"
                                         :source "github.com/example/utils"
                                         :ref nil
                                         :commit "a91c04f0")
                                :conflicts nil))))
              (list "an invalid requirement scope is rejected"
                    '(:plugins-lock
                      :version 1
                      :plugins
                      ((:plugin :name "utils"
                                :source "github.com/example/utils"
                                :ref nil
                                :commit "a91c04f0"
                                :checkout "plugins/checkouts/utils/"
                                :dependencies nil
                                :winner (:requirement
                                         :scope :bogus
                                         :requester nil
                                         :source "github.com/example/utils"
                                         :ref nil
                                         :commit "a91c04f0")
                                :conflicts nil))))
              (list "a plugin-scope requirement without a requester is rejected"
                    '(:plugins-lock
                      :version 1
                      :plugins
                      ((:plugin :name "utils"
                                :source "github.com/example/utils"
                                :ref nil
                                :commit "a91c04f0"
                                :checkout "plugins/checkouts/utils/"
                                :dependencies nil
                                :winner (:requirement
                                         :scope :plugin
                                         :requester nil
                                         :source "github.com/example/utils"
                                         :ref nil
                                         :commit "a91c04f0")
                                  :conflicts nil))))
              (list "a dependency without a commit is rejected"
                    '(:plugins-lock
                      :version 1
                      :plugins
                      ((:plugin :name "utils"
                                :source "github.com/example/utils"
                                :ref nil
                                :commit "a91c04f0"
                                :checkout "plugins/checkouts/utils/"
                                :dependencies ((:dependency
                                                :name "logging"
                                                :source "github.com/example/logging"))
                                :winner nil
                                :conflicts nil))))))
      (destructuring-bind (description form) case
        (test-assert (not (plugin-lock-form-p form)) description)))))

(-> test-plugin-lock-file () null)
(defun test-plugin-lock-file ()
  "Test lock reading, missing files, writing, and typed malformed conditions."
  (with-test-configuration (configuration)
    (let ((pathname (plugin-lock-path configuration))
          (form (plugin-tests--lock-form)))
      (test-assert (plugin-lock-empty-p (plugin-lock-read pathname))
                   "a missing lock reads as empty")
      (plugin-lock-write (form->plugin-lock form) pathname)
      (test-assert (probe-file pathname)
                   "the writer publishes plugins.lock")
      (test-assert (equal (plugin-lock->form (plugin-lock-read pathname)) form)
                   "the written lock reads back as the same form")
      (plugin-lock-save configuration (make-instance 'plugin-lock))
      (test-assert (plugin-lock-empty-p (plugin-lock-load configuration))
                   "an empty lock round-trips")
      (snapshot-write pathname '(:plugins-lock :version 1 :plugins 3))
      (test-assert
       (handler-case
           (progn (plugin-lock-read pathname) nil)
         (plugin-lock-format-error (condition)
           (and (eq (plugin-lock-error-operation condition) ':read)
                (equal (plugin-lock-error-pathname condition) pathname)
                (stringp (plugin-lock-format-error-reason condition))
                (not (null (plugin-lock-format-error-form condition))))))
       "malformed lock data raises a typed condition with structured data")
      (snapshot-write-text pathname "(:plugins-lock")
      (test-assert
       (handler-case
           (progn (plugin-lock-read pathname) nil)
         (plugin-lock-error (condition)
           (and (eq (plugin-lock-error-operation condition) ':read)
                (equal (plugin-lock-error-pathname condition) pathname)
                (not (null (plugin-lock-error-cause condition))))))
       "unreadable lock data raises a typed read condition"))))

(-> test-plugin-image-layers () null)
(defun test-plugin-image-layers ()
  "Test the three image rungs as data."
  (let* ((global (plugin-tests--identity "111111"
                                        :source "github.com/example/global"))
         (site (plugin-tests--identity "222222"
                                       :source "github.com/example/site"))
         (project (plugin-tests--identity "333333"
                                          :source "github.com/example/project"))
         (layer-1 (plugin-image-layer-1 "source-commit"
                                         :mutation-commit "mutation-commit"))
         (layer-2 (plugin-image-layer-2 "source-commit"
                                         :mutation-commit "mutation-commit"
                                         :global-plugins (list global)
                                         :site-plugins (list site)))
         (layer-3 (plugin-image-layer-3 "source-commit" "/work/project"
                                         :mutation-commit "mutation-commit"
                                         :global-plugins (list global)
                                         :site-plugins (list site)
                                         :project-plugins (list project))))
    (test-assert (and (= (plugin-image-layer-rung layer-1) 1)
                      (null (plugin-image-layer-plugins layer-1)))
                 "rung 1 is tracked source plus the mutation commit")
    (test-assert (equal (plugin-image-layer-plugins layer-2)
                        (list global site))
                 "rung 2 adds global then site plugins")
    (test-assert (equal (plugin-image-layer-plugins layer-3)
                        (list global site project))
                 "rung 3 adds one project's plugins")
    (test-assert (and (= (plugin-image-layer-rung layer-3) 3)
                      (string= (plugin-image-layer-project layer-3)
                               "/work/project"))
                 "rung 3 names its project")
    (test-assert
     (handler-case
         (progn (plugin-image-layer-2 "source-commit"
                                       :global-plugins (list "not-an-identity"))
                nil)
       (plugin-image-layer-error () t))
     "a layer rejects a plugin list that holds no identities")))
