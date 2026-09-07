(in-package #:autolith)

;;;; -- Preloaded Active Image Tests --

(-> test-active-image-build-record () null)
(defun test-active-image-build-record ()
  "Test exact source identity, runtime compatibility, and manifest projection."
  (let* ((source-root (asdf:system-source-directory :autolith))
         (record (active-image-build-record-create source-root))
         (source-files (getf (rest record) :source-files))
         (probe (active-image-probe-record record))
         (manifest (active-image-manifest-form
                    #P"/tmp/autolith-active-test.core"
                    record)))
    (test-assert (active-image-build-record-p record)
                 "active-image build records are complete portable data")
    (test-assert (active-image-build-record-compatible-p record source-root)
                 "the active-image record matches its exact source and runtime")
    (test-assert (equal (mapcar #'first source-files)
                        (active-image-source-paths source-root))
                 "active-image identities cover every compiled source input")
    (test-assert (and (eq (first probe) :autolith-active-image)
                      (= (getf (rest probe) :version)
                         *active-image-protocol-version*))
                 "the active-image probe exposes the current protocol")
    (test-assert (and (eq (first manifest) :active-image)
                      (equal (getf (rest manifest) :source-files)
                             source-files))
                 "active-image manifests retain exact source identities")
    (let ((wrong-source (copy-tree record)))
      (setf (second (first (getf (rest wrong-source) :source-files)))
            "0000000000000000000000000000000000000000")
      (test-assert
       (not (active-image-build-record-compatible-p wrong-source source-root))
       "active-image compatibility rejects a changed source blob"))
    (let ((wrong-runtime (copy-tree record)))
      (setf (getf (rest wrong-runtime) :sbcl-version) "0.0.0")
      (test-assert
       (not (active-image-build-record-compatible-p wrong-runtime source-root))
       "active-image compatibility rejects another SBCL runtime"))
    (let ((wrong-os-version (copy-tree record)))
      (setf (getf (rest wrong-os-version) :operating-system-version) "0.0.0")
      (test-assert
       (not (active-image-build-record-compatible-p wrong-os-version source-root))
       "active-image compatibility rejects another OS version")))
  nil)

(-> test-image-commit-replay-probe () null)
(defun test-image-commit-replay-probe ()
  "Test clean-process loading and rejection of private replay scripts."
  (let* ((configuration (test-configuration))
         (root (test-configuration-root configuration))
         (identifier (make-identifier))
         (script (merge-pathnames "probe/reconstruct.lisp" root)))
    (unwind-protect
         (progn
           (image-commit-write-script
            script :identifier identifier
            :title (format nil "Multiline metadata~%(error \"Executed title.\")")
            :entries
            (list
             (list :kind ':definition :id "generic"
                   :target "(defgeneric image-commit-test-operation)"
                   :source "(defgeneric image-commit-test-operation (value))")
             (list :kind ':definition
                   :id (format nil "method~%(error \"Executed identifier.\")")
                   :target (format nil "(defmethod image-commit-test-operation nil~%  (integer))")
                   :source "(defmethod image-commit-test-operation ((value integer)) (+ value 7))")
             (list :kind ':legacy :id "assertion" :target "result"
                   :source "(assert (= 42 (image-commit-test-operation 35)))")))
           (test-assert
            (null (image-commit-replay-probe configuration script identifier))
            "generated replay executes methods without evaluating multiline metadata")
           (delete-file script)
           (with-open-file (stream script
                                   :direction ':output
                                   :if-exists ':supersede
                                   :if-does-not-exist ':create
                                   :external-format ':utf-8)
             (format stream "(in-package #:autolith)~%(error \"Broken replay.\")~%"))
           (test-assert
            (handler-case
                (progn
                  (image-commit-replay-probe configuration script identifier)
                  nil)
              (image-commit-error (condition)
                (and (eq (image-commit-error-stage condition) ':replay-probe)
                     (search "Broken replay."
                             (autolith-error-message condition)))))
            "a rejected replay script carries the probe output in its error")
           (let ((log-pathname
                   (merge-pathnames
                    "replay-probe.log"
                    (uiop:pathname-directory-pathname script))))
             (test-assert
              (and (probe-file log-pathname)
                   (search "Broken replay."
                           (uiop:read-file-string log-pathname)))
              "a rejected replay probe persists its complete output beside the script")))
      (uiop:delete-directory-tree root :validate t :if-does-not-exist ':ignore)))
  nil)
