(in-package #:autolith)

;;;; -- Preloaded Active Image --

;;; sbcl-generations builds, probes and installs the image; this file names its
;;; inputs, its entry point, the SBCL that boots it, and the platform steps that
;;; publish it.

(defparameter *active-image-probe-argument*
  "--autolith-internal-active-image-probe"
  "The private argument requesting active-image validation, which the launchers pass.")

;;; The pinned image library has no provenance callback. Adapt only its Git
;;; command boundary, so its install, fresh saver and saved-core probes share
;;; the same jj identities without changing the build-record protocol.
(eval-when (:load-toplevel :execute)
  (load (asdf:system-relative-pathname :autolith "script/source-provenance.lisp")))

(defvar *active-image-git-provenance-function*
  (symbol-function 'sbcl-generations::image--git-output)
  "The image library's unmodified provenance implementation.")

(defun active-image--source-command (source-root arguments)
  "Use jj provenance in jj workspaces and the original library otherwise."
  (cl-user::autolith-source-command source-root arguments
                                   *active-image-git-provenance-function*))

(eval-when (:load-toplevel :execute)
  (setf (symbol-function 'sbcl-generations::image--git-output)
        #'active-image--source-command))

(-> active-image-source-paths (pathname) list)
(defun active-image-source-paths (source-root)
  "Return sorted repository-relative inputs compiled into an active image."
  (let* ((source-directory (merge-pathnames "src/" source-root))
         (lisp-paths
           (mapcar (lambda (pathname)
                     (enough-namestring pathname source-root))
                   (source-lisp-pathnames source-directory))))
    (sort (append '("bin/autolith"
                    "bin/autolith-active"
                    "bin/autolith-search-worker"
                    "bin/autolith-runtime"
                    "script/build-active"
                    "script/build-active.lisp"
                    "script/restart-publisher.lisp"
                    "script/source-provenance.lisp"
                    "autolith.asd"
                    "qlfile"
                    "qlfile.lock"
                    "sbcl.version")
                  lisp-paths)
          #'string<)))

(-> active-image-build-record-create (pathname) list)
(defun active-image-build-record-create (source-root)
  "Return the exact source and runtime identity for a new active image of SOURCE-ROOT."
  (active-image--call-translating-errors
   (lambda ()
     (image-build-record (uiop:ensure-directory-pathname source-root)
                         :inputs #'active-image-source-paths))))

(-> active-image-build-record-compatible-p (t pathname) boolean)
(defun active-image-build-record-compatible-p (record source-root)
  "Return true when RECORD exactly matches SOURCE-ROOT and this runtime."
  (handler-case
      (image-build-record-compatible-p
       record
       (uiop:ensure-directory-pathname (platform-truename *platform* source-root))
       :inputs #'active-image-source-paths)
    (error ()
      nil)))


;;;; -- Installed Image Selection --

(-> active-image-installed-build-record (pathname) (option list))
(defun active-image-installed-build-record (core-pathname)
  "Return the build record in CORE-PATHNAME's manifest, or NIL when it is unusable."
  (image-installed-record core-pathname :read-function #'active-image--read-manifest))

(-> active-image-current-core (configuration) (option pathname))
(defun active-image-current-core (configuration)
  "Return CONFIGURATION's installed active core when it matches the source, else NIL.

The manifest beside the core carries the build record that the core embeds, so
the exact source and runtime comparison needs no extra boot of the image."
  (let ((core (config :active-image-core configuration)))
    (and (active-image-build-record-compatible-p (active-image-installed-build-record core)
                                                 (config :source-root configuration))
         (probe-file core)
         core)))

(-> active-image-process-command (configuration list) list)
(defun active-image-process-command (configuration arguments)
  "Return the argv running a fresh Autolith with command-line ARGUMENTS.

The process boots the current active core when one matches the source, which
takes a fraction of a second, and otherwise loads the system from source."
  (let* ((sbcl-command (active-image--sbcl-command))
         (source-root (config :source-root configuration))
         (core (active-image-current-core configuration)))
    (if core
        (list* sbcl-command
               "--noinform"
               "--core" (namestring core)
               "--end-runtime-options"
               (namestring source-root)
               arguments)
        (list* sbcl-command
               "--noinform"
               "--script"
               (namestring (merge-pathnames "bin/autolith-active" source-root))
               arguments))))

(-> active-image--sbcl-command () string)
(defun active-image--sbcl-command ()
  "Return the SBCL that boots active images: AUTOLITH_SBCL, or sbcl from PATH."
  (let ((configured-command (uiop:getenv "AUTOLITH_SBCL")))
    (if (non-empty-string-p configured-command)
        configured-command
        "sbcl")))

(-> active-image--read-manifest (pathname) t)
(defun active-image--read-manifest (pathname)
  "Return the single form of the active-image manifest at PATHNAME."
  (multiple-value-bind (form sole-form-p) (snapshot-read pathname)
    (unless sole-form-p
      (error 'active-image-build-error
             :message "The active-image manifest does not hold exactly one form."
             :stage ':manifest
             :pathname pathname))
    form))


;;;; -- Image Entry and Publication --

(-> active-image--toplevel (list) null)
(defun active-image--toplevel (arguments)
  "Run Autolith in a booted active image whose first of ARGUMENTS is its source root."
  (handler-case
      (let ((source-root (and arguments
                              (uiop:ensure-directory-pathname (pathname (first arguments))))))
        (unless source-root
          (error 'active-image-build-error
                 :message "The preloaded active image needs its source root."
                 :stage ':entry
                 :pathname nil))
        (platform-setenv "AUTOLITH_SOURCE_ROOT" (namestring source-root))
        (restart-case
            (main (rest arguments))
          (abort ()
            :report "Exit the preloaded Autolith image."
            nil)))
    (serious-condition (condition)
      (format *error-output* "Autolith's preloaded active image failed: ~A~%" condition)
      (uiop:quit 1)))
  nil)

(-> active-image--prepare-saver () null)
(defun active-image--prepare-saver ()
  "Clear this process's session and credential state before it is saved."
  (setf *active-application* nil
        *credentials-in-request-scope* nil
        *active-secret-use-count* 0
        *secret-use-depth* 0
        *secret-use-quiescence-owner* nil)
  nil)

(-> active-image-save (pathname pathname) null)
(defun active-image-save (source-root pathname)
  "Save this process as the preloaded active image of SOURCE-ROOT at PATHNAME.

The build record is computed here, so it names exactly the source this process
loaded; the process exits inside the save. Hosts without fork build their image
this way, from a fresh process that script/build-active.lisp starts."
  (image-save pathname
              (active-image-build-record-create
               (uiop:ensure-directory-pathname (platform-truename *platform* source-root)))
              :inputs         #'active-image-source-paths
              :toplevel       #'active-image--toplevel
              :prepare        #'active-image--prepare-saver
              :probe-argument *active-image-probe-argument*)
  nil)

(-> active-image--fresh-process-command (pathname pathname) list)
(defun active-image--fresh-process-command (source-root temporary)
  "Return the argv that loads SOURCE-ROOT in a new SBCL and saves it at TEMPORARY."
  (list (uiop:native-namestring sb-ext:*runtime-pathname*)
        "--noinform"
        "--script" (uiop:native-namestring
                    (merge-pathnames "script/build-active.lisp" source-root))
        "--child" (uiop:native-namestring temporary)))

(-> active-image--publish-core (pathname pathname) null)
(defun active-image--publish-core (temporary core-pathname)
  "Replace CORE-PATHNAME with the probed TEMPORARY core and make it read-only."
  (platform-replace-file *platform* temporary core-pathname)
  (platform-make-read-only *platform* core-pathname)
  nil)

(-> active-image--write-manifest (pathname list) pathname)
(defun active-image--write-manifest (pathname form)
  "Atomically replace PATHNAME with the read-only active-image manifest FORM."
  (snapshot-write pathname form :mode #o444))

(-> active-image--call-translating-errors (function) t)
(defun active-image--call-translating-errors (function)
  "Call FUNCTION, reporting sbcl-generations image failures as ACTIVE-IMAGE-BUILD-ERROR."
  (handler-case
      (funcall function)
    (sbcl-generations:checkpoint-error (condition)
      (error 'active-image-build-error
             :message  (sbcl-generations:checkpoint-error-message condition)
             :stage    (sbcl-generations:checkpoint-error-stage condition)
             :pathname (sbcl-generations:checkpoint-error-pathname condition)))))

(-> active-image-install
    (pathname pathname &key (:saver (member :automatic :fresh-process)))
    pathname)
(defun active-image-install (source-root core-pathname &key (saver ':automatic))
  "Build, validate, and atomically install a preloaded active image.

SAVER :AUTOMATIC forks this process where the host can; :FRESH-PROCESS always
loads the system in a new SBCL, so the image holds nothing else from this heap."
  (let ((source-root (uiop:ensure-directory-pathname
                      (platform-truename *platform* source-root))))
    (active-image--call-translating-errors
     (lambda ()
       (image-install source-root (pathname core-pathname)
                      :inputs           #'active-image-source-paths
                      :toplevel         #'active-image--toplevel
                      :prepare          #'active-image--prepare-saver
                      :saver            (if (platform-supports-p *platform* ':forked-image-saver)
                                            saver
                                            ':fresh-process)
                      :fresh-process-command
                      (lambda (temporary)
                        (active-image--fresh-process-command source-root temporary))
                      :probe-runner     (make-sbcl-core-probe-runner
                                         :command (active-image--sbcl-command))
                      :probe-argument   *active-image-probe-argument*
                      :publish-function #'active-image--publish-core
                      :write-function   #'active-image--write-manifest)))))
