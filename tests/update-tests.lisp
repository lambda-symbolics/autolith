(in-package #:autolith)

;;;; -- Update State and Provenance Tests --

(-> update-tests--write-release-record (pathname string) pathname)
(defun update-tests--write-release-record (pathname tag)
  "Write a strict release record carrying TAG to PATHNAME."
  (ensure-directories-exist pathname)
  (with-open-file (stream pathname
                          :direction ':output
                          :if-exists ':supersede
                          :if-does-not-exist ':create
                          :external-format ':utf-8)
    (format stream "version=~A~%tag=~A~%commit=~A~%"
            (subseq tag 1)
            tag
            "0123456789abcdef0123456789abcdef01234567"))
  pathname)

(-> test-update-state-and-installation-provenance () null)
(defun test-update-state-and-installation-provenance ()
  "Test strict cached availability, dismissal, refresh, and trusted provenance."
  (let* ((configuration (test-configuration))
         (root (test-configuration-root configuration))
         (state-path (configuration-update-state-path configuration))
         (release-tag (format nil "v~A" *autolith-version*))
         (newer-tag "v99.0.0")
         (install-root (merge-pathnames "installation/" root))
         (release-root (merge-pathnames
                        (format nil "releases/~A/" release-tag)
                        install-root))
         (packaged-source (merge-pathnames "libexec/autolith/" release-root)))
    (unwind-protect
         (progn
           (test-assert (release-tag-valid-p "v1.2.3")
                        "strict release tags accept three numeric components")
           (dolist (tag '("1.2.3" "v1.2" "v1.2.3.4" "v1.2.x" "v01..2"))
             (test-assert (not (release-tag-valid-p tag))
                          (format nil "malformed release tag ~S is rejected" tag)))
           (test-assert (release-tag< "v1.9.9" "v2.0.0")
                        "release ordering compares numeric components")
           (test-assert (not (release-tag< "v2.0.0" "v1.99.99"))
                        "release ordering never treats an older major as newer")

           (let ((source (installation-provenance-detect
                          configuration
                          :kind "release"
                          :release-root (namestring root))))
             (test-assert (eq (installation-provenance-method source) ':source)
                          "a release environment marker alone remains source"))
           (test-assert
            (and (installation--nix-store-directory-p
                  #p"/nix/store/0123456789-autolith/")
                 (not (installation--nix-store-directory-p
                       #p"/tmp/nix/store/0123456789-autolith/")))
            "Nix provenance accepts only concrete paths below the Nix store")

           (ensure-directories-exist (merge-pathnames ".keep" packaged-source))
           (update-tests--write-release-record
            (merge-pathnames "RELEASE" release-root)
            release-tag)
           (ensure-directories-exist (merge-pathnames ".keep" install-root))
           (with-test-fixture (':symbolic-links "packaged release provenance")
             (test-fixture-make-symbolic-link
              *platform*
              (format nil "releases/~A" release-tag)
              (uiop:native-namestring (merge-pathnames "current" install-root)))
             (let* ((packaged-configuration
                      (make-instance
                       'configuration
                       :source-root packaged-source
                       :working-directory packaged-source
                       :config-root (configuration-config-root configuration)
                       :data-root (configuration-data-root configuration)
                       :state-root (configuration-state-root configuration)
                       :cache-root (configuration-cache-root configuration)
                       :codex-auth-path
                       (configuration-codex-auth-path configuration)
                       :model *default-model*
                       :reasoning-effort *default-reasoning-effort*
                       :provider-endpoint *codex-responses-endpoint*))
                    (release
                      (installation-provenance-detect
                       packaged-configuration
                       :kind "release"
                       :release-root (namestring release-root))))
               (test-assert (eq (installation-provenance-method release) ':release)
                            "selected packaged topology validates release provenance")
               (test-assert
                (string= (installation-provenance-current-tag release) release-tag)
                "release provenance carries its validated current tag")

               (update-state--write
                configuration
                (make-instance 'update-state
                               :last-attempt-at 100
                               :last-success-at 100
                               :latest-tag newer-tag))
               (test-assert
                (not (update-state-check-due-p
                      (update-state-load configuration)
                      :now 101
                      :interval 20))
                "a fresh cache suppresses another network request")
               (test-assert
                (update-state-check-due-p
                 (update-state-load configuration)
                 :now 120
                 :interval 20)
                "a stale cache permits another bounded request")
               (test-assert
                (string=
                 (update-availability-tag
                  (update-availability-current configuration release))
                 newer-tag)
                "a newer cached release becomes startup availability")
               (update-state-dismiss configuration newer-tag)
               (test-assert
                (null (update-availability-current configuration release))
                "skipping one exact cached version suppresses its notice")
               (update-state--record-success configuration 130 "v100.0.0")
               (test-assert
                (update-availability-current configuration release)
                "a later release becomes visible despite an older dismissal")))

           (snapshot-write state-path '(:update-state :version 999))
           (let ((state (update-state-load configuration)))
             (test-assert
              (and (null (update-state-latest-tag state))
                   (null (update-state-last-attempt-at state)))
              "malformed cached state degrades to safe empty state"))

           (let* ((fetches 0)
                  (*update-check-fetch-function*
                    (lambda ()
                      (incf fetches)
                      "v101.2.3")))
             (test-assert (update-state-refresh configuration :now 200)
                          "a due successful refresh reports success")
             (let ((state (update-state-load configuration)))
               (test-assert
                (and (= (update-state-last-attempt-at state) 200)
                     (= (update-state-last-success-at state) 200)
                     (string= (update-state-latest-tag state) "v101.2.3"))
                "successful refresh atomically caches timing and release data"))
             (test-assert
              (not (update-state-refresh configuration :now 201))
              "an ordinary refresh respects a fresh cache")
             (test-assert
              (update-state-refresh configuration :now 201 :force-p t)
              "an explicit refresh bypasses a fresh cache")
             (let ((state (update-state-load configuration)))
               (test-assert
                (and (= fetches 2)
                     (= (update-state-last-attempt-at state) 201)
                     (= (update-state-last-success-at state) 201)
                     (string= (update-state-latest-tag state) "v101.2.3"))
                "an explicit refresh records its fresh release result")))
           (let ((*update-check-fetch-function*
                   (lambda () (error "offline"))))
             (let ((failed-at (+ 201 *update-check-interval*)))
               (test-assert
                (not (update-state-refresh configuration :now failed-at))
                "a network failure is nonfatal")
               (let ((state (update-state-load configuration)))
                 (test-assert
                  (and (= (update-state-last-attempt-at state) failed-at)
                       (= (update-state-last-success-at state) 201)
                       (string= (update-state-latest-tag state) "v101.2.3"))
                  "failed refresh retains the last valid release cache")))))
      (platform-delete-directory-tree *platform* root :validate t :if-does-not-exist ':ignore)))
  nil)
