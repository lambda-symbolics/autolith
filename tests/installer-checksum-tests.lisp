(in-package #:autolith)

;;;; -- Installer Archive Verification --

(-> test-installer-checksum-verification () null)
(defun test-installer-checksum-verification ()
  "Test exact archive verification before extraction or installation selection."
  (with-test-configuration (configuration root)
    (declare (ignore configuration))
    (let* ((source-root       (asdf:system-source-directory :autolith))
           (release-root      (merge-pathnames "release/" root))
           (fixtures          (merge-pathnames "downloads/" root))
           (commands          (merge-pathnames "commands/" root))
           (installation      (merge-pathnames "installation/" root))
           (current           (merge-pathnames "current" installation))
           (extraction-marker (merge-pathnames "extracted" root))
           (installer         (merge-pathnames "script/install" source-root))
           (tar-command       (release-archive--command-pathname "tar"))
           (environment
             (list (format nil "PATH=~A:~A" commands (uiop:getenv "PATH"))
                   (format nil "AUTOLITH_TEST_RELEASE_FIXTURE=~A" fixtures)
                   (format nil "AUTOLITH_INSTALL_ROOT=~A" installation)
                   "AUTOLITH_RELEASE_BASE_URL=https://example.invalid")))
      (release-script-tests--make-release source-root release-root)
      (release-script-tests--install-linux-host-tools commands)
      (test-write-text
       (merge-pathnames "curl" commands) (release-script-tests--fixture-curl))
      (release-script-tests--chmod "755" (merge-pathnames "curl" commands))
      (labels ((install (tag)
                 (release-script-tests--run
                  (list (namestring installer) "--without-command-link" "--version" tag)
                  :environment environment :ignore-error-status t :output nil))

               (reject ()
                 (multiple-value-bind (output diagnostics status) (install "v1.0.1")
                   (declare (ignore output diagnostics))
                   (test-assert (not (eql status 0))
                                "an unverifiable download fails installation"))
                 (test-assert (not (probe-file extraction-marker))
                              "verification failure occurs before archive extraction")
                 (test-assert
                  (string= (release-script-tests--readlink current)
                           "releases/v1.0.0-x86_64-linux")
                  "verification failure preserves the selected release")))
        (unwind-protect
             (progn
               (release-script-tests--write-release-archive
                release-root fixtures :tag "v1.0.0" :platform "x86_64-linux"
                                      :record-platform "x86_64-linux")
               (multiple-value-bind (output diagnostics status) (install "v1.0.0")
                 (declare (ignore output))
                 (test-assert (eql status 0) (format nil "valid archive installs: ~A" diagnostics)))
               (multiple-value-bind (archive checksum)
                   (release-script-tests--write-release-archive
                    release-root fixtures :tag "v1.0.1" :platform "x86_64-linux"
                                          :record-platform "x86_64-linux")
                 (let* ((valid-checksum (uiop:read-file-string checksum))
                        (digest         (subseq valid-checksum 0 64))
                        (filename       (file-namestring archive)))
                   (test-write-text
                    (merge-pathnames "tar" commands)
                    (format nil "#!/bin/sh~%touch ~A~%exec ~A \"$@\"~%"
                            (uiop:escape-shell-token (namestring extraction-marker))
                            (uiop:escape-shell-token (namestring tar-command))))
                   (release-script-tests--chmod "755" (merge-pathnames "tar" commands))
                   (dolist (invalid
                            (list "" "not a checksum"
                                  (format nil "~A  ~A~%" (make-string 64 :initial-element #\0) filename)
                                  (format nil "~A  /dev/null~%"
                                          "e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855")
                                  (format nil "~A  ../~A~%" digest filename)
                                  (concatenate 'string valid-checksum valid-checksum)))
                     (test-write-text checksum invalid)
                     (reject))
                   (delete-file checksum)
                   (reject)
                   (test-write-text checksum valid-checksum)
                   (with-open-file (stream archive :direction ':output :if-exists ':append
                                                  :element-type '(unsigned-byte 8))
                     (write-byte 0 stream))
                   (reject)
                   (release-script-tests--write-checksum archive checksum)
                   (setf digest (subseq (uiop:read-file-string checksum) 0 64))
                   (test-write-text
                    checksum (format nil "~A *~A~%" (string-upcase digest) filename))
                   (multiple-value-bind (output diagnostics status) (install "v1.0.1")
                     (declare (ignore output))
                     (test-assert (eql status 0)
                                  (format nil "binary-style SHA-256 record installs: ~A" diagnostics)))
                   (test-assert (probe-file extraction-marker)
                                "a verified archive proceeds to extraction"))))
          (release-script-tests--cleanup root)))))
  nil)
